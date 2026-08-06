"""Acceptance test for the R-side parity fixture export.

Proves the fixtures load from Python with the values R wrote --
bit-exact for the float64 matrix, exactly-round-tripped for the
17-digit CSV numerics, and with the FORCED categorical ordering that
determines site order (and therefore which site is the lead
decryptor). Reference values were printed from R independently.

Run: uv run --with numpy --with pandas python verify_fixtures.py <dir>
"""

import gzip
import hashlib
import json
import sys
from pathlib import Path

import numpy as np
import pandas as pd

D = Path(sys.argv[1] if len(sys.argv) > 1 else ".")
man = json.loads((D / "manifest.json").read_text())
entries = {f["file"]: f for f in man["files"]}
fails = []


def check(label, ok, detail=""):
    print(f"  {'PASS' if ok else 'FAIL'}  {label}{'  ' + detail if detail else ''}")
    if not ok:
        fails.append(label)


print(f"manifest schema v{man['schema_version']} | homomorpheR "
      f"{man['homomorpheR']} | openfhe.R {man['openfhe_R']} | R {man['R_version']}")

print("\n[1] integrity: every declared file present with matching sha256")
for name, e in entries.items():
    p = D / name
    if not p.exists():
        check(name, False, "MISSING")
        continue
    got = hashlib.sha256(p.read_bytes()).hexdigest()
    check(name, got == e["sha256"] and p.stat().st_size == e["bytes"],
          f"{e['bytes']}B")

if fails:
    # Stop here: every downstream check reads these bytes, so continuing
    # would either crash on malformed data or report confusing cascades.
    print(f"\nINTEGRITY FAILURE ({len(fails)}): {fails}")
    print("Fixtures are stale or corrupt. Regenerate with "
          "fixtures/sync_fixtures.sh")
    sys.exit(1)

print("\n[2] DLBCL_gex: raw float64 round-trip (bit-exact vs R)")
e = entries["dlbcl_gex.f64.gz"]
nrow, ncol = e["shape"]
raw_bytes = gzip.open(D / "dlbcl_gex.f64.gz", "rb").read()
check("uncompressed content sha256 matches manifest",
      hashlib.sha256(raw_bytes).hexdigest() == e["sha256_content"])
gex = np.frombuffer(raw_bytes, dtype="<f8").reshape(nrow, ncol)
check("shape", gex.shape == (235, 6416), str(gex.shape))
# Reference values printed from R at 17 significant digits.
check("gex[0,0:3]", np.array_equal(gex[0, :3],
                                   np.array([-0.221, -0.17860000000000001,
                                             -0.050250000000000003])),
      str(gex[0, :3]))
check("gex[234,6415] == 1.2190000000000001",
      gex[234, 6415] == 1.2190000000000001, repr(gex[234, 6415]))
# colSums feeds the univariate screen; must match R bit-for-bit at the
# rank-100 boundary, so check the sum itself, not a rounded version.
cs = gex.sum(axis=0)
check("colSums[0] == 7.7198723849372453",
      cs[0] == 7.7198723849372453, repr(cs[0]))
check("colSums[1] == 3.7857083682008352",
      cs[1] == 3.7857083682008352, repr(cs[1]))

rn = (D / "dlbcl_gex_rownames.txt").read_text().split()
cn = (D / "dlbcl_gex_colnames.txt").read_text().split()
check("dimnames lengths", len(rn) == nrow and len(cn) == ncol,
      f"{len(rn)}x{len(cn)}")

print("\n[3] DLBCL clinical: forced dtypes, no inference")
ce = entries["dlbcl_clinical.csv"]
raw = pd.read_csv(D / "dlbcl_clinical.csv", dtype=str)
df = pd.DataFrame(index=raw.index)
for col, spec in ce["dtypes"].items():
    if isinstance(spec, dict) and spec["dtype"] == "category":
        df[col] = pd.Categorical(raw[col], categories=spec["categories"],
                                 ordered=spec["ordered"])
    elif spec == "int64":
        df[col] = raw[col].astype("int64")
    elif spec == "float64":
        df[col] = raw[col].astype("float64")
    else:
        df[col] = raw[col]

check("shape", df.shape == (ce["nrow"], ce["ncol"]), str(df.shape))
check("time[0:3] exact float64",
      list(df["time"][:3]) == [4.0, 4.9000000000000004, 5.5999999999999996],
      str(list(df["time"][:3])))
check("ID is int64", df["ID"].dtype == "int64")

print("\n[4] site order is FORCED, not alphabetical (protocol semantics)")
cats = list(df["Subgroup"].cat.categories)
check("categories == R level order", cats == ["GCB", "ABC", "Type III"], str(cats))
check("NOT alphabetical (would permute the lead decryptor)",
      cats != sorted(cats), f"sorted would be {sorted(cats)}")
sizes = {k: int(v) for k, v in ce["site_sizes"].items()}
observed = {c: int((df["Subgroup"] == c).sum()) for c in cats}
check("site sizes match manifest", observed == sizes, str(observed))
# A naive groupby would silently reorder; show that we detect it.
naive = list(df.groupby("Subgroup", observed=True, sort=True).groups.keys())
check("groupby(sort=True) preserves forced order (categorical dtype)",
      list(naive) == cats, str(list(naive)))

print("\n[5] gex row names align with clinical IDs (Cox-lasso invariant)")
check("as.character(ID) == rownames(gex)",
      [str(i) for i in df["ID"]] == rn)

print("\n[6] simulated inputs (unreproducible from numpy -- fixture is the only path)")
mle = json.loads((D / "mle_poisson.json").read_text())
check("mle: 40 Poisson counts", len(mle["y"]) == 40 and all(
    isinstance(v, int) for v in mle["y"]), f"first 5 {mle['y'][:5]}")
check("mle: site split covers all 40",
      sum(len(s["y"]) for s in mle["sites"]) == 40)

qc = json.loads((D / "query_count.json").read_text())
tot = qc["expected"]["total"]
per = qc["expected"]["per_site"]
check("query-count: per-site sums to total", sum(per) == tot,
      f"{per} -> {tot}")
check("query-count: declared exact (BFV)", qc["expected"]["exact"] is True)
# Recompute the query in pandas from the shipped rows: the exported
# expectation must be reproducible from the exported data.
recomputed = []
for site in qc["sites"]:
    s = pd.DataFrame(site)
    recomputed.append(int(((s["age"] < 50) & (s["sex"] == "F")
                           & (s["bm"] < 0.2)).sum()))
check("query-count: pandas reproduces R's per-site counts",
      recomputed == per, f"{recomputed} vs {per}")

er = json.loads((D / "encrypted_regression.json").read_text())
check("encrypted-regression: n=500 aligned",
      len(er["age"]) == len(er["biomarker"]) == len(er["outcome"]) == 500)

agg = json.loads((D / "aggregation_sites.json").read_text())
check("aggregation: site sizes 1000/500/1500",
      [len(s["age"]) for s in agg["sites"]] == [1000, 500, 1500])

print("\n[7] cvxr_consensus golden fixture + tolerance ladder")
g = json.loads((D / "cvxr_consensus_golden.json").read_text())
check("K == 100 and top_idx length matches", g["params"]["K"] == 100
      and len(g["top_idx"]) == 100)
check("n_iter_enc == n_iter_ref == 147",
      g["n_iter_enc"] == g["n_iter_ref"] == 147)
check("trajectory has n_iter entries of length K",
      len(g["trajectory"]) == 147 and len(g["trajectory"][0]) == 100)
z_enc, z_ref = np.array(g["z_enc"]), np.array(g["z_ref"])
d = np.max(np.abs(z_enc - z_ref))
check("shipped z_enc vs z_ref within its CKKS tolerance",
      d <= g["tolerances"]["z_enc_vs_z_ref"]["value"], f"{d:.3g}")
tol = g["tolerances"]
check("tolerance ladder distinguishes solver from CKKS",
      tol["z_ref_vs_python"]["kind"] == "statistical"
      and tol["z_enc_vs_z_ref"]["kind"] == "ckks"
      and tol["top_idx"]["kind"] == "set_equality")

print("\n" + ("ALL CHECKS PASSED" if not fails
              else f"{len(fails)} FAILURES: {fails}"))
sys.exit(1 if fails else 0)
