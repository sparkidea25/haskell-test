{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- module Todo.Api where
    module Todo.Api
  ( TodoService
  , TodoApi
  , IndexTodo
  , ShowTodo
  , CreateTodo
  , UpdateTodo
  , DeleteTodo
  ) where

    import Servant.API
    import Todo.Types (NewTodoDto, EditTodoDto)

    -- type NewTodoDto = TodoDto Identity
    type IndexTodo = Get '[JSON] [NewTodoDto]

    type ShowTodo
        = Capture "todoId" Integer
        :> Get '[JSON] NewTodoDto

    type CreateTodo
        = ReqBody '[JSON] NewTodoDto
        :> PostCreated '[JSON] NewTodoDto


    type UpdateTodo
        = Capture "todoId" Int
        :> ReqBody '[JSON] EditTodoDto
        :> Put '[JSON] NewTodoDto

    type DeleteTodo
        = Capture "todoId" Int
        :> Delete '[JSON] NoContent

    type TodoApi = "todos" :> IndexTodo
            :<|> "todos" :> ShowTodo
            :<|> "todos" :> CreateTodo
            :<|> "todos" :> UpdateTodo
            :<|> "todos" :> DeleteTodo


    type TodoService = "todo" :> TodoApi