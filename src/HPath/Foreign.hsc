module HPath.Foreign where

#include <limits.h>

pathMax :: Int
pathMax = #{const PATH_MAX}

