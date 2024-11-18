import React from 'react'
import ReactDOM from 'react-dom/client'
import {NavBar, TodoApp} from './sections'
import './index.css'
import './color.css'

const initialTasks = [];


ReactDOM.createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <NavBar/>
    <TodoApp 
      initialTasks={initialTasks} 
      initialFilter={"All"}
      />
  </React.StrictMode>,
)
