import {useState, useEffect} from "react"; 
import {nanoid} from "nanoid";

import {Todo, Form, FilterButton} from "./Components";


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

    function fetchServerTasks() {
          fetch('http://localhost:8080/stmGet')
            .then(response => response.json())
            .then(data => setTasks( data ))
            .catch(error => {
              console.error('Error: ', error)
            }
            );
      }
    useEffect(() => {fetchServerTasks()}, []);

    function postTodo (newTodo) {
        fetch("http://localhost:8080/stmPost", {
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
        console.log(JSON.stringify(newTodo))
    }

    const [serverStatusMsg, setserverStatusMsg] = useState('');

    function fetchServerStatusMsg() {
        
            fetch('http://localhost:8080/serverConnected')
                .then(response => response.json())
                .then(data => setserverStatusMsg(data))
                .catch(error => {
                    console.error('Error: ', error)
                    setserverStatusMsg('not connected')
                }
                );
    }
    useEffect(() => {fetchServerStatusMsg()}, []);

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
    }

    function editTask(id, newName) {
        function editIfEdited (task) {
            if (task.id === id) {
                return { ...task, name: newName };
            } else return task;
        }
        const revisedTasks = tasks.map(editIfEdited);
        setTasks(revisedTasks);
    }

    function deleteTask(id) {
        const remainingTasks = tasks.filter((task) => task.id !== id);
        setTasks(remainingTasks);
    }

    
    // Visuals
    const headingText = `${tasks.filter(FILTER_MAP["Active"]).length} tasks remaining`;
    
    return (
        <>
            <div className="todoapp stack-large content">
                <h1>TodoMatic v1</h1>
                <p>Server status: {serverStatusMsg}</p>

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
  