{-# LANGUAGE DataKinds #-}

module Todo.Types 
  ( TodoDto(..)
  , NewTodoDto
  , EditTodoDto
  ) where

import Data.Functor.Identity (Identity)

data TodoDto f = TodoDto
  { todoTitle   :: f String
  , todoContent :: f String
  }

type NewTodoDto = TodoDto Identity
type EditTodoDto = TodoDto Maybe
