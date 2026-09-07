# Add a site to a non-cooperating party

Part of the frozen Paillier-era legacy surface.

## Usage

``` r
add_site(ncp, ...)
```

## Arguments

- ncp:

  an
  [NCParty](https://bnaras.github.io/homomorpheR/reference/NCParty.md).

- ...:

  method-specific arguments. The NCParty method takes a single
  [Site](https://bnaras.github.io/homomorpheR/reference/Site.md).

## Value

the [NCParty](https://bnaras.github.io/homomorpheR/reference/NCParty.md)
`ncp`, invisibly. Called for its side effect: the site is appended to
the list of sites the party manages, held in its `state` environment.
