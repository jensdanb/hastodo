
const hsUrl = 'http://localhost:8080'

async function getJSON  (url) {
    const response = await fetch(url);
  
    if (!response.ok) {
      throw new Error(`HTTP error: Status ${response.status}`);
    }
  
    return response.json();
  };

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