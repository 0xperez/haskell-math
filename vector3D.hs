--This simple module provides an algebraic data type for a vector in three dimensions, along with functions to perform
--common arithmetical operation with vectors and conversions between the three most common coordinate systems in which
--they are used, that is the Cartesian, spherical and cylindrical coordinate system.

{--------------------------------------------------------------------------------------------------------------------}
module Vectors3D where

    data Vector3D = Vector3D Double Double Double deriving (Eq)
    
    instance Show (Vector3D) where
        show (Vector3D i j k) = "(" ++  show i ++ ", " ++ show j ++ ", " ++ show k ++ ")"
    

{--The basic algebraic operations of sum/difference and multiplication are defined in the Num typeclass, along with
several other numerical functions such as the absolute value and the sign function. By making this class an instance
of Num, this results in an overloading of such operators, that can be applied to vectors as they were "native types"
themselves (recall that (+), (*) and so on are nothing else than infix functions).-}

    instance Num (Vector3D) where

--Summing two vectors results in a third vector whose components are the sum of the two vectors' components
        (Vector3D i j k) + (Vector3D l m n) = Vector3D (i+l) (j+m) (k+n)
        (Vector3D i j k) - (Vector3D l m n) = Vector3D (i-l) (j-m) (k-n)

--Multiply two vectors a and b to compute their cross product a x b. The result is another vector which is 
--perpendicular to both and normal to their plane (see https://en.wikipedia.org/wiki/Cross_product a detailed explanation)
        (Vector3D i j k) * (Vector3D l m n) =
            let x = (j*n - m*k)
                y = (k*l - n*i)
                z = (i*m - l*j)
            in 
                Vector3D x y z

--Applying the absolute value function to a vector results in a vector whose new components are the absolute values of the
--previous ones.
        abs (Vector3D i j k) = Vector3D (abs i) (abs j) (abs k)

--Applying the sign function to a vector results in a vector whose each component is either 1 or -1, depending on
--their sign. 
        signum (Vector3D i j k) = Vector3D (signum i) (signum j) (signum k)

--In Haskell, numeric literals are just sintactic sugar for fromInteger: 5 is actually seen as fromInteger 5.
--Some common functions, such as sum, require a fromInteger implementation to be called.
--However, if not called inside a function which declare types, a call to fromInteger n would just return n as default behavior.
--To apply it to a vector, you need to explicit its types as fromInteger n :: Vector3D 
        fromInteger n =
            let 
                x = (fromInteger n)
            in
                Vector3D x x x
{-------------------------------------------------------------------------------------------------------------------}
{----Basic operations----}

--Returns the vector's length (or norm or magnitude).
    norm :: Vector3D -> Double
    norm (Vector3D i j k) =
        sqrt (i*i + j*j + k*k)


--Multiplies a vector for a scalar number. Such number may be an integer or any real number as well.
    scale :: Real c => Vector3D -> c -> Vector3D
    scale (Vector3D i j k) c =
        let toDouble :: (Real n) => n -> Double
            toDouble = fromRational . toRational

            l = toDouble c*i
            m = toDouble c*j
            n = toDouble c*k
        in
          Vector3D l m n


--Computes the dot product between two vectors. See https://en.wikipedia.org/wiki/Dot_product for a detailed explanation.
    dot :: Vector3D -> Vector3D -> Double
    dot (Vector3D i1 j1 k1) (Vector3D i2 j2 k2) = 
        i1*i2 + j1*j2 + k1*k2

{--------------------------------------------------------------------------------------------------------------------}
{---Operations derived from the common ones---}

--Computes three vector's triple product. The triple product of a, b, c is defined as a vector d = a . (b x c) where . is 
--the dot product and x the cross product. See more at https://en.wikipedia.org/wiki/Triple_product.
    triple :: Vector3D -> Vector3D -> Vector3D -> Double
    triple a b c =
        dot a (b*c)


--Computes the angle between two vectors using their dot product.
    angle :: Vector3D -> Vector3D -> Double
    angle a b = 
        let prod = dot a b
            norms = norm a * norm b
        in acos (prod/norms)

--Normalizes a vector, i.e. converts it to a unit vector.
    normalize :: Vector3D -> Vector3D
    normalize v@(Vector i j k) =
        let
            n = norm v
        in
            Vector (i/n) (j/n) (k/n)


--Returns a vector whose components are the direction cosines of the first vector, i.e. the cosines of the angles 
--between the vector and the three coordinate axes. See https://en.wikipedia.org/wiki/Direction_cosine for details.
    directionCos :: Vector3D -> Vector3D
    directionCos (Vector3D i j k) =
        Vector3D (cos i) (cos j) (cos k)


--Returns the Euclidean distance between two vectors.
    distance :: Vector3D -> Vector3D -> Double
    distance (Vector3D i j k) (Vector3D l m n) =
        let
            dx = (l-i)
            dy = (m-j)
            dz = (n-k)
        in
            sqrt(dx*dx + dy*dy + dz*dz)

{--------------------------------------------------------------------------------------------------------------------}
{-Orthogonality and parallelism test-}

--Tests whether two vectors are orthogonal, i.e. perpendicular
    areOrthogonal :: Vector3D -> Vector3D -> Bool
    areOrthogonal a b = 
        dot a b == 0

--Tests whether two vectors are parallel
    areParallel :: Vector3D -> Vector3D -> Bool
    areParallel a b = 
        a*b == 0

{--------------------------------------------------------------------------------------------------------------------}
{---Conversion of vectors in different coordinate systems----}

--Given a vector in an orthonormal Cartesian coordinate system, these functions performs a conversion into an 
--equivalent vector in a cylindrical coordinate system and viceversa. See more on https://en.wikipedia.org/wiki/Cylindrical_coordinate_system.
    cartesianToCylind :: Vector3D -> Vector3D
    cartesianToCylind (Vector3D i j k) =
        let
            rho = sqrt (i*i + j*j)
            phi = atan (j/i)
        in
            Vector3D rho phi k

    cylindToCartesian :: Vector3D -> Vector3D
    cylindToCartesian (Vector3D rho phi k) =
        let
            i = rho * (cos phi)
            j = rho * (sin phi)
        in
            Vector3D i j k


--Given a vector in a cartesian coordinate system, these functions convert them into an equivalent vector in a
--spherical coordinate system and viceversa. See more on https://en.wikipedia.org/wiki/Spherical_coordinate_system.
    cartesianToSpherical :: Vector3D -> Vector3D
    cartesianToSpherical (Vector3D i j k) =
        let
            r = sqrt (i*i + j*j + k*k)
            theta = acos (k/r)
            phi = atan (j/i)
        in
            Vector3D r theta phi


    sphericalToCartesian :: Vector3D -> Vector3D
    sphericalToCartesian (Vector3D r theta phi) =
        let
            i = r * (sin theta) * (cos phi)
            j = r * (sin theta) * (sin phi)
            k = r * (cos theta)
        in
            Vector3D i j k


--Returns in form of a list the three vectors (versors) forming the Euclidean space.
    standardBasis :: [Vector3D]
    standardBasis = [Vector3D 1 0 0, Vector3D 0 1 0, Vector3D 0 0 1]

{--------------------------------------------------------------------------------------------------------------------}
{-Differential calculus-}
--Given a function with three variables and a point, computes the gradient of the function in such point. The function is expressed as an
--Haskell function which takes as an input a triple of doubles and returns a double, and the point is expressed as a Vector3D instance.
--function computes the gradient of the function in the point given by the vector. This is done by exploiting Newton's difference quotient in the 3-dimensional case.
--Note that, since we are working with a limit involving h->0, the result is an approximation of such 
    gradient :: ((Double, Double, Double) -> Double) -> Vector3D -> Vector3D
    gradient f (Vector3D x y z) =
            let 
                h = 0.00001
                dx = (f (x+h, y, z) - f (x,y,z)) / h
                dy = (f (x, y+h, z) - f (x,y,z)) / h
                dz = (f (x, y, z+h) - f (x,y,z)) / h
            in
                Vector dx dy dz



{--------------------------------------------------------------------------------------------------------------------}
{--Vector generation and conversion --}

--Given a function Double -> Double and a vector, returns a vector whose components are the result of the function
--application to each component of the input vector
    transform :: (Double -> Double) -> Vector3D -> Vector3D
    transform f (Vector3D i j k) = 
        Vector3D (f i) (f j) (f k)


--Given a list of triples of type double, converts them in a list of corresponding vectors.
    toVectors :: [(Double, Double, Double)] -> [Vector3D]
    toVectors list =
        let
            toVec (x, y, z) = Vector3D x y z
        in
            map toVec list