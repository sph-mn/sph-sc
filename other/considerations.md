# why write variable types after identifiers
* variable name centric vs data type centric
* type as a property subordinate to the name

one one hand, something like this
~~~
16 bit var1
32 bit var2
16 bit var3
64 bit var4
~~~

at the begining of functions seems practical - set up the stack and register the space needed by local variables.
but on the other hand, the variable types dont seem to matter that much to a user and the identifiers are more important.

~~~
var1 16 bit
var2 32 bit
var3 16 bit
var4 64 bit
~~~
