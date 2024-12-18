import { useState, useEffect } from "react"; 
import { nanoid } from "nanoid";
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query'

import { setServerData, getTodos, postTodo, putTodo, delTodo } from "../../networking"
import { Todo, Form, FilterButton, ConnectionStatus } from "./Components";


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

    const settleTodos = () => {queryClient.invalidateQueries({ queryKey: ['todos'] })}
    
    const addTodoMutation = useMutation({
        mutationFn: (name) => {
            const newTask = { id: `todo-${nanoid()}`, name, completed: false };
            return postTodo(newTask)
        }, 
        onSettled: settleTodos,
      })

    const delTodoMutation = useMutation({
        mutationFn: (id) => {return delTodo(id)},
        onSettled: settleTodos,
      })
    
    const putTodoMutation = useMutation({
        mutationFn: putTodo,
        onSettled: settleTodos,
        mutationKey: ['putTodo']
      })
    const { isPending, submittedAt, variables, mutate, isError } = putTodoMutation
        
        
    const [taskFilter, setTaskFilter] = useState(initialFilter);

    const taskList = todos.data
        ?.filter(FILTER_MAP[taskFilter])
        ?.map((task) => (
            <Todo
                id={task.id}
                name={task.name}
                completed={task.completed}
                key={task.id}
                toggleTaskCompleted={putTodoMutation.mutate}
                editTask={putTodoMutation.mutate}
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
            <ConnectionStatus/>

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
  