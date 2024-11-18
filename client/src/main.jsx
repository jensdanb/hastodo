import React from 'react'
import ReactDOM from 'react-dom/client'
import {NavBar, TodoApp} from './sections'
import './index.css'
import './color.css'

const initialTasks = [
  { id: "todo-0", name: "Eat", completed: true },
  { id: "todo-1", name: "Sleep", completed: false },
  { id: "todo-2", name: "Repeat", completed: false },
];


ReactDOM.createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <NavBar/>
    <TodoApp 
      initialTasks={initialTasks} 
      initialFilter={"All"}
      />
  </React.StrictMode>,
)
