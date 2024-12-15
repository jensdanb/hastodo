
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

function setServerData(address, setter) {
    fetch(hsUrl + address)
        .then(response => response.json())
        .then(data => setter( data ))
        .catch(error => {
        console.error('Error: ', error)
        }
        );
}

export {getJSON, hsUrl, setServerData}