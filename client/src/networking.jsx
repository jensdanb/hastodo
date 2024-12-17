
const hsUrl = 'http://localhost:8080'

async function getJSON (address) {
    const response = await 
      fetch(hsUrl + address)
      .catch(error => {
        console.error('Error: ', error)
      });
  
    if (!response.ok) {
      throw new Error(`HTTP error: Status ${response.status}`);
    }
    else {return response.json();}
  };

async function getTodos() {
  return (await getJSON('/getTodos'))
}

async function modifyingQuery (address, clientTodo) {
  var method = ""
  if (["/postTodo"].includes(address)) {
    method = "POST"
  }
  else if (["/putTodo"].includes(address)) {
    method = "PUT"
  }
  else if (["/delTodo"].includes(address)) {
    method = "DELETE"
  }
  else {throw new Error('Could not resolve HTTP method from address: ' + address)}

  const response = await fetch(hsUrl + address, {
          method: method,
          body: JSON.stringify(clientTodo),
          headers: {
              "Content-type": "application/json; charset=UTF-8"
          }
      })
      .catch(error => {
          console.error('Error: ', error)
      }
      );
  return response.json()
}

async function postTodo(newTodo) {
  await modifyingQuery('/postTodo', newTodo);
}

async function putTodo({id, toggle, newName}) {
  await modifyingQuery('/putTodo', [id, toggle, newName]);
}

async function delTodo(id) {
  await modifyingQuery('/delTodo', id);
}

function setServerData(address, setter) {
    fetch(hsUrl + address)
        .then(response => response.json())
        .then(data => setter( data ))
        .catch(error => {
        console.error('Error: ', error)
        }
        );
}

export {getJSON, hsUrl, setServerData, getTodos, postTodo, putTodo, delTodo}