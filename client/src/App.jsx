import { useState, useEffect } from 'react'
import reactLogo from './assets/react.svg'
import viteLogo from '/vite.svg'
import './App.css'

function App() {
  const [count, setCount] = useState(0)

  const [message, setMessage] = useState('');

  function fetchServerStatusMsg() {
    useEffect(() => {
      fetch('http://localhost:8080/serverConnected')
        .then(response => response.json())
        .then(data => setMessage(data))
        .catch(error => {
          console.error('Error: ', error)
          setMessage('not connected')
        }
        );
    }, []);
  }
  fetchServerStatusMsg()

  return (
    <>
      <div>
        <a href="https://vitejs.dev" target="_blank">
          <img src={viteLogo} className="logo" alt="Vite logo" />
        </a>
        <a href="https://react.dev" target="_blank">
          <img src={reactLogo} className="logo react" alt="React logo" />
        </a>
      </div>
      <h1>Vite + React</h1>

      <div className="card">
        <button onMouseDown={() => setCount((count) => count + 1)}>
          count is {count}
        </button>
        <p>
          Edit <code>src/App.jsx</code> and save to test HMR
        </p>
      </div>

      <p className="read-the-docs">
        Click on the Vite and React logos to learn more
      </p>

      <div>
        <h1>Message from Haskell Server:</h1>
        <p>{message}</p>

    </div>
    </>
  )
}

export default App


