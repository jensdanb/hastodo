import { useState, useEffect } from "react"; 
import { nanoid } from "nanoid";
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'

import { getTodos, postTodo, postTodos, putTodo, delTodo } from "../../services/networking"
import { Todo, Form, FilterButton, PwaController } from "./Components";
import { getUnsyncedTodos } from '.../services/local_db';


const FILTER_MAP = {
    All: () => true,
    Active: (task) => !task.completed,
    Completed: (task) => task.completed,
  };

const FILTER_NAMES = Object.keys(FILTER_MAP);

function TodoApp({initialFilter}) {
    // State

    const queryClient = useQueryClient()

    const todos = useQuery({ queryKey: ['todos'], queryFn: getTodos })

    const invalidateTodos = () => {queryClient.invalidateQueries({ queryKey: ['todos'] })}

    const addTodoMutation = useMutation({
        mutationFn: (name) => {
            const newTask = { id: `todo-${nanoid()}`, name: name, completed: false, reachedServer: true };
            return postTodo(newTask)
        },
        onSettled: invalidateTodos,
      })
    // const { postIsPending, postSubmittedAt, postVariables, postMutate, postIsError } = postTodoMutation

    const delTodoMutation = useMutation({
        mutationFn: (id) => {return delTodo(id)},
        onSettled: invalidateTodos,
      })
    

    // const { isPending, submittedAt, variables, mutate, isError } = putTodoMutation
        
        
    const [taskFilter, setTaskFilter] = useState(initialFilter);

    const taskList = todos.data
        ?.filter(FILTER_MAP[taskFilter])
        ?.map((task) => (
            <Todo
                id={task.id}
                name={task.name}
                completed={task.completed}
                key={task.id + task.name + task.completed}
                invalidateTodoList={invalidateTodos}
                deleteTask={delTodoMutation.mutate}
            />)
        );
    
    const filterButtons = FILTER_NAMES.map((name) => 
        <FilterButton 
            key={name} 
            name={name}
            isPressed={name === taskFilter}
            setTaskFilter={setTaskFilter}
            />
    );

    // Visuals
    const headingText = `${todos.data?.length} tasks, ${todos.data?.filter(FILTER_MAP["Active"]).length} remaining`;
    
    return (
        <>
        <div className="todoapp stack-large content">
            <h1>TodoMatic v2</h1>
            <PwaController/>

            <Form onSubmit={addTodoMutation.mutate}/>

            <div className="filters btn-group stack-exception">
                {filterButtons}
            </div>
            
            <h2 id="list-heading">{headingText}</h2>
            <ul
                role="list"
                className="todo-list stack-large stack-exception"
                aria-labelledby="list-heading">
                {taskList}
            </ul>
        </div>
        </>
    );
  }
  
  export default TodoApp;
  