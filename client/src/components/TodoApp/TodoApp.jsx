import { useState, useEffect } from "react"; 
import { nanoid } from "nanoid";
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';

import { getTodos, postTodo, postTodos, putTodo, delTodo } from "../../services/networking";
import { cacheTodoList } from "../../services/databasing";
import { Todo, Form, FilterButton, PwaController } from "./Components";



const FILTER_MAP = {
    All: () => true,
    Active: (task) => !task.completed,
    Completed: (task) => task.completed,
  };

const FILTER_NAMES = Object.keys(FILTER_MAP);

function TodoApp({initialFilter}) {

    const [intendedOnline, setIntendedOnline] = useState(true);
    const [actualOnline, setActualOnline] = useState(false);
    

    const queryClient = useQueryClient()

    // todos :: [{ completed :: bool, id :: string, name :: string, knownUnSynced :: bool }]
    const todos = useQuery({ 
        queryKey: ['todos'], 
        queryFn: async () => {
            return await getTodos()
                .then((todos) => {
                    setActualOnline(true);
                    return todos;
                })
                .catch(() => {
                    setActualOnline(false);
                    cacheTodoList(todos.data);
                });
            }
        });

    const invalidateTodos = () => {queryClient.invalidateQueries({ queryKey: ['todos'] })}

    const addTodoMutation = useMutation({
        mutationFn: (name) => {
            const newTask = { id: `todo-${nanoid()}`, name: name, completed: false, knownUnSynced: true };
            return postTodo(newTask)
        },
        onSettled: invalidateTodos,
      });
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
                knownUnSynced={task.knownUnSynced}
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
            <PwaController 
                intendedOnline={intendedOnline} 
                toggleOnline={() => {
                    if (!intendedOnline) {invalidateTodos()}
                    setIntendedOnline(intendedOnline ? false : true)
                }}
                actualOnline={actualOnline} 
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
  