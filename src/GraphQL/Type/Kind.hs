module GraphQL.Type.Kind
  ( TypeKind(..)
  ) where


data TypeKind
  = SCALAR
  | OBJECT
  | INTERFACE
  | UNION
  | ENUM 
  | INPUT_OBJECT
  | LIST_OF TypeKind
  | NON_NULL TypeKind
