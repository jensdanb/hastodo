import React from 'react'
import ReactDOM from 'react-dom/client'

import App from './App';
import './index.css';
import './color.css';
import { createTodoDB } from "./services/databasing";
import { cacheServerTodos } from './services/common';

createTodoDB();
cacheServerTodos();

ReactDOM.createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <App/>
  </React.StrictMode>,
)
