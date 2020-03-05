
# adding radians to the trigo functions to prevent any misunderstanding
sinradian = sin
asinradian = asin
cosradian = cos
acosradian = acos
tanradian = tan
atanradian = atan

randbetween = function(
   minLimit,
   maxLimit,
   n
) {
   minLimit + ( runif(n) * ( maxLimit - minLimit ) )
}

# Can add new functions here -
# NewFunction = function( ... ) { ... }
# Add details of this function in Metadata/Operations.csv