import { useEffect, useState } from 'react';
import { getJSON, hsUrl, setServerData } from '../../../networking';



function ConnectionStatus () {
    const [connectionStatus, setConnectionStatus] = useState('');

    useEffect(() => {setServerData('/serverConnected', setConnectionStatus)}, []);

    return (
        <p>Server status: {connectionStatus}</p>
    )
}

export default ConnectionStatus;