# 0.7.4.0

* Update to `primitive-0.7.4.0`

# 0.7.3.0

* Update to `primitive-0.7.3.0`
* `unsafeFreezeByteArray` and `resizeMutableByteArray` now intentionally corrupt the original array by writing `0xFF` to every byte

# 0.7.2.0

* Update to `primitive-0.7.2.0`
* Add missing checks for `copyPrimArrayToPtr` and `copyMutablePrimArrayToPtr`
* Make error messages for `writeByteArray`, `copyPrimArray` and `copyMutablePrimArray` more descriptive
