import {useState, useEffect} from "react"; 
import {nanoid} from "nanoid";

import {setServerData} from "../../networking"
import {Todo, Form, FilterButton, ConnectionStatus} from "./Components";


const FILTER_MAP = {
    All: () => true,
    Active: (task) => !task.completed,
    Completed: (task) => task.completed,
  };

const FILTER_NAMES = Object.keys(FILTER_MAP);

function TodoApp({initialTasks, initialFilter}) {
    // State

    const [tasks, setTasks] = useState(initialTasks);

    const [taskFilter, setTaskFilter] = useState(initialFilter);

    const taskList = tasks
        .filter(FILTER_MAP[taskFilter])
        ?.map((task) => (
            <Todo
                id={task.id}
                name={task.name}
                completed={task.completed}
                key={task.id}
                toggleTaskCompleted={toggleTaskCompleted}
                editTask={editTask}
                deleteTask={deleteTask}
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

    useEffect(() => {setServerData('/getTodos', setTasks)}, []);

    function postTodo (newTodo) {
        fetch("http://localhost:8080/postTodo", {
                method: "POST",
                body: JSON.stringify(newTodo),
                headers: {
                    "Content-type": "application/json; charset=UTF-8"
                }
            })
            .catch(error => {
                console.error('Error: ', error)
            }
            );
    }

    function serverDeleteTodo (id) {
        fetch("http://localhost:8080/delTodo", {
                method: "DELETE",
                body: JSON.stringify(id),
                headers: {
                    "Content-type": "application/json; charset=UTF-8"
                }
            })
            .catch(error => {
                console.error('Error: ', error)
            }
            );
    }

    function serverPutTodo (id, toggle, newName) {
        fetch("http://localhost:8080/putTodo", {
                method: "PUT",
                body: JSON.stringify([id, toggle, newName]),
                headers: {
                    "Content-type": "application/json; charset=UTF-8"
                }
            })
            .catch(error => {
                console.error('Error: ', error)
            }
            );
    }

    // Functions
    function addTask(name) {
        const newTask = { id: `todo-${nanoid()}`, name, completed: false };
        setTasks([...tasks, newTask]);
        postTodo(newTask)
    }

   
    function toggleTaskCompleted(id) {
        function toggleIfToggled (task) {
            if (task.id === id) {
                return { ...task, completed: !task.completed };
            } else return task; 
        }
        const revisedTasks = tasks.map(toggleIfToggled);
        setTasks(revisedTasks);
        serverPutTodo(id, true, "");
    }

    function editTask(id, newName) {
        function editIfEdited (task) {
            if (task.id === id) {
                return { ...task, name: newName };
            } else return task;
        }
        const revisedTasks = tasks.map(editIfEdited);
        setTasks(revisedTasks);
        serverPutTodo(id, false, newName);
    }

    function deleteTask(id) {
        const remainingTasks = tasks.filter((task) => task.id !== id);
        setTasks(remainingTasks);
        serverDeleteTodo(id)
    }

    
    // Visuals
    const headingText = `${tasks.length} tasks, ${tasks.filter(FILTER_MAP["Active"]).length} remaining`;
    
    return (
        <>
            <div className="todoapp stack-large content">
                <h1>TodoMatic v2</h1>
                <ConnectionStatus/>

                <Form onSubmit={addTask}/>

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
  