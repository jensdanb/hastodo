import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query'

import { getJSON, hsUrl, setServerData } from '../../../networking';



function ConnectionStatus () {
    const {data: connectionStatus, isLoading, error} = useQuery({ 
        queryKey: ['connectionStatusQueryKey'], 
        queryFn: () => getJSON('/serverConnected')
    });

    if (isLoading) return <div>Trying to reach server...</div>;
    if (error) return <div>An error occurred: {error.message}</div>;

    return (
        <p>Server status: {connectionStatus}</p>
    )
}

export default ConnectionStatus;