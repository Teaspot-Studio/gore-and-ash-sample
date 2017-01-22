How to use template
===================

The template with `.hsfiles` extension is created with `MakeTemplates.hs` script.
To generate a new `.hsfiles` just run in console the following:

``` bash
./MakeTemplate.hs
```

And generate new template with:

``` bash
stack new gore-and-ash-mymodule ./gore-and-ash-sample.hsfiles \
  -p module-name:Test \
  -p module-docs-name:test \
  -p field-prefix:test \
  -p github-username:teaspot-studio \
  -p current-year:2017 \
  -p author-name:ncrashed \
  -p author-email:ncrashed@gmail.com \
  -p copyright:Anton Gushcha 2017 \
  --solver
```
