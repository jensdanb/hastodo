import { useState, useEffect } from "react"; 
import { nanoid } from "nanoid";
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';

import { netGetTodos, netPostTodo, netDelTodo, netPostTodos } from "../../services/networking";
import { cacheTodoList, dbGetTodoList, dbAddTodo, dbPutTodo, dbDelTodo } from "../../services/databasing";
import { mutationFunction } from "../../services/common";
import { Todo, Form, FilterButton, PwaController } from "./Components";



const FILTER_MAP = {
    All: () => true,
    Active: (task) => !task.completed,
    Completed: (task) => task.completed,
  };

const FILTER_NAMES = Object.keys(FILTER_MAP);

function TodoApp({initialFilter}) {

    const [onlineMode, setOnlineMode] = useState(true);
    const [actualOnline, setActualOnline] = useState(false);

    const queryClient = useQueryClient()

    // todos :: [{ completed :: bool, id :: string, name :: string, knownUnSynced :: bool }]
    const todos = useQuery({ 
        queryKey: ['todos'], 
        queryFn: dbGetTodoList
    });

    const invalidateTodos = () => {queryClient.invalidateQueries({ queryKey: ['todos'] })}
    

    const addTodoMutation = useMutation({
        mutationFn: async (name) => {
            const newTask = { id: `todo-${nanoid()}`, name: name, completed: false, knownUnSynced: true }; 
            return await mutationFunction(newTask, netPostTodo, dbAddTodo);
        },
        onSettled: invalidateTodos,
    });

    const delTodoMutation = useMutation({
        mutationFn: async (id) => {
            return await netDelTodo(id)
                .then(() => {dbDelTodo(id)})
        },
        onSettled: invalidateTodos,
      })
    

    // const { isPending, submittedAt, variables, mutate, isError } = putTodoMutation
        
        
    const [taskFilter, setTaskFilter] = useState(initialFilter);
    
    function todoComponent(todoData) { 
        return <Todo
                    id={todoData.id}
                    key={todoData.id}
                    name={todoData.name}
                    completed={todoData.completed}
                    knownUnSynced={todoData.knownUnSynced}
                    todoData={todoData}
                    invalidateTodoList={invalidateTodos}
                    deleteTask={delTodoMutation.mutate}
                />
    }

    const taskList = todos.data
        ?.filter(FILTER_MAP[taskFilter])
        ?.map(todoComponent);
    
    const filterButtons = FILTER_NAMES.map((name) => 
        <FilterButton 
            key={name} 
            name={name}
            isPressed={name === taskFilter}
            setTaskFilter={setTaskFilter}
            />
    );

    const headingText = `${todos.data?.length} tasks, ${todos.data?.filter(FILTER_MAP["Active"]).length} remaining`;
    
    return (
        <>
        <div className="todoapp stack-large content">
            <h1>TodoMatic v2</h1>
            <PwaController 
                intendedOnline={onlineMode} 
                toggleOnline={() => {
                    if (!onlineMode) {invalidateTodos()}
                    setOnlineMode(onlineMode ? false : true)
                }}
                //actualOnline={actualOnline} 
                />

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
  