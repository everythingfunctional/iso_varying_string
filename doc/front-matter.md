---
project: iso_varying_string
summary: An implementation of the ISO_VARYING_STRING module as proposed for the ISO standard.
project_website: https://gitlab.com/everythingfunctional/iso_varying_string
project_download: https://gitlab.com/everythingfunctional/iso_varying_string/-/releases
author: Brad Richardson
email: everythingfunctional@protonmail.com
website: https://everythingfunctional.com
twitter: https://twitter.com/everythingfunct
github: https://github.com/everythingfunctional
src_dir: ../src
display: public
         protected
         private
sort: permission-alpha
output_dir: ../public
graph: true
...

This module defines facilities in Fortran for the manipulation of
character strings of dynamically variable length. It is in conformance
with the [ISO/IEC 1539-2: 2000](http://www.astro.wisc.edu/~townsend/resource/download/code/Fortran-ISO_VARYING_STRING.pdf)
extension to the Fortran Standard.

Neither the internal representation of the derived type, nor the
algorithms used to implement the procedures or operators are directed
by the standard, and as such should not be relied upon by any user of
this module.

It should be noted that this module defines facilities for dynamically
varying length strings of characters of default kind only. Similar
facilities could be defined for non-default kind characters by a
separate, if similar, module for each such character kind.

This module has been designed, as far as is reasonable, to provide for
varying length character strings the facilities that are available for
intrinsic fixed length character strings. All the intrinsic operations
and functions that apply to fixed length character strings have extended
meanings defined by this module for varying length character strings.
Also, a small number facilities are defined that are appropriate because
of the essential differences between the intrinsic type and the varying
length derived data type.

NOTE: The standard does not specify that the `varying_string` type must
be usable in `print`, `read` or `write` statements. This implementation
provides such functionality as an extension.