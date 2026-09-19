# Package index

## All functions

- [`CKKSMaster()`](https://bnaras.github.io/homomorpheR/reference/CKKSMaster.md)
  : CKKS-backed master

- [`DLBCL`](https://bnaras.github.io/homomorpheR/reference/DLBCL.md) :
  Diffuse Large B-cell Lymphoma Cohort (Rosenwald et al. 2002)

- [`DLBCL_gex`](https://bnaras.github.io/homomorpheR/reference/DLBCL_gex.md)
  : DLBCL Lymphochip gene-expression matrix

- [`LocalSite()`](https://bnaras.github.io/homomorpheR/reference/LocalSite.md)
  :

  A site whose data lives in this R session

- [`Master()`](https://bnaras.github.io/homomorpheR/reference/Master.md)
  : Abstract master class

- [`NCParty()`](https://bnaras.github.io/homomorpheR/reference/NCParty.md)
  : A non-cooperating party

- [`PaillierCiphertext()`](https://bnaras.github.io/homomorpheR/reference/PaillierCiphertext.md)
  : A Paillier ciphertext

- [`PaillierEncryptedReal()`](https://bnaras.github.io/homomorpheR/reference/PaillierEncryptedReal.md)
  : A Paillier-encrypted real number

- [`PaillierKeyPair()`](https://bnaras.github.io/homomorpheR/reference/PaillierKeyPair.md)
  : Paillier key pair

- [`PaillierMaster()`](https://bnaras.github.io/homomorpheR/reference/PaillierMaster.md)
  : Paillier-backed master

- [`PaillierPrivateKey()`](https://bnaras.github.io/homomorpheR/reference/PaillierPrivateKey.md)
  : Paillier private key

- [`PaillierPublicKey()`](https://bnaras.github.io/homomorpheR/reference/PaillierPublicKey.md)
  : Paillier public key

- [`RemoteSite()`](https://bnaras.github.io/homomorpheR/reference/RemoteSite.md)
  :

  A site whose contribution is produced outside this R session

- [`Site()`](https://bnaras.github.io/homomorpheR/reference/Site.md) : A
  site in a multi-party protocol

- [`ThresholdMaster()`](https://bnaras.github.io/homomorpheR/reference/ThresholdMaster.md)
  : Threshold-CKKS master (n-of-n key generation)

- [`add_site()`](https://bnaras.github.io/homomorpheR/reference/add_site.md)
  : Add a site to a non-cooperating party

- [`contribute()`](https://bnaras.github.io/homomorpheR/reference/contribute.md)
  : A site's encrypted contribution at a parameter value

- [`cvxr_consensus`](https://bnaras.github.io/homomorpheR/reference/cvxr_consensus.md)
  : Precomputed encrypted Cox-lasso consensus-ADMM results

- [`decrypt()`](https://bnaras.github.io/homomorpheR/reference/decrypt.md)
  : Decrypt a Paillier ciphertext

- [`encrypt()`](https://bnaras.github.io/homomorpheR/reference/encrypt.md)
  : Encrypt a value under a Paillier public key

- [`encrypt_real()`](https://bnaras.github.io/homomorpheR/reference/encrypt_real.md)
  : Encrypt a real number under a Paillier public key

- [`encrypt_under()`](https://bnaras.github.io/homomorpheR/reference/encrypt_under.md)
  : Encrypt a value under the public parameters a party holds

- [`get_lambda()`](https://bnaras.github.io/homomorpheR/reference/get_lambda.md)
  : Return the secret lambda from a private key

- [`get_private_key()`](https://bnaras.github.io/homomorpheR/reference/get_private_key.md)
  : Return the private key from a key pair

- [`homomorpheR-package`](https://bnaras.github.io/homomorpheR/reference/homomorpheR.md)
  [`homomorpheR`](https://bnaras.github.io/homomorpheR/reference/homomorpheR.md)
  : homomorpheR: Homomorphic computations in R

- [`keygen_round()`](https://bnaras.github.io/homomorpheR/reference/keygen_round.md)
  : One site's step in the threshold key-generation chain

- [`make_ckks_master()`](https://bnaras.github.io/homomorpheR/reference/make_ckks_master.md)
  : Construct a CKKS-backed master

- [`make_master()`](https://bnaras.github.io/homomorpheR/reference/make_master.md)
  : Construct a Paillier-backed master

- [`make_ncparty()`](https://bnaras.github.io/homomorpheR/reference/make_ncparty.md)
  : Construct an NCParty

- [`make_threshold_master()`](https://bnaras.github.io/homomorpheR/reference/make_threshold_master.md)
  : Run threshold key generation across sites and construct the master

- [`make_worker()`](https://bnaras.github.io/homomorpheR/reference/make_worker.md)
  : Construct a worker

- [`master_aggregate()`](https://bnaras.github.io/homomorpheR/reference/master_aggregate.md)
  : Run one round of the master/worker protocol

- [`master_decrypt()`](https://bnaras.github.io/homomorpheR/reference/master_decrypt.md)
  : Decrypt the master's protocol result back to a scalar real

- [`paillier_keypair()`](https://bnaras.github.io/homomorpheR/reference/paillier_keypair.md)
  : Generate a new Paillier key pair

- [`partial_decrypt()`](https://bnaras.github.io/homomorpheR/reference/partial_decrypt.md)
  : One site's partial decryption of a ciphertext

- [`random.bigz()`](https://bnaras.github.io/homomorpheR/reference/random.bigz.md)
  : Random big integer

- [`round_robin_chain()`](https://bnaras.github.io/homomorpheR/reference/round_robin_chain.md)
  : Wire a master and a list of sites into a round-robin chain

- [`run_round_robin()`](https://bnaras.github.io/homomorpheR/reference/run_round_robin.md)
  : Run one round of the round-robin protocol

- [`set_next_site()`](https://bnaras.github.io/homomorpheR/reference/set_next_site.md)
  :

  Wire one site's `next_site` to another

- [`set_public_key()`](https://bnaras.github.io/homomorpheR/reference/set_public_key.md)
  : Distribute the public key from the master to a downstream actor

- [`set_workers()`](https://bnaras.github.io/homomorpheR/reference/set_workers.md)
  : Wire a master to a flat list of workers

- [`site_unavailable()`](https://bnaras.github.io/homomorpheR/reference/site_unavailable.md)
  : Signal that a site could not be reached
