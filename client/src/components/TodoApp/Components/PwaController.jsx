import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import 'idb';

import { getJSON, hsUrl } from '../../../networking';



function PwaController () {
    const [intendedOnline, setIntendedOnline] = useState(true);
    if (intendedOnline) return <OnlinePwa/>; 
    else return <OfflinePwa/>;
}

function OnlinePwa (){
    const {data: actualOnline, isLoading, error} = useQuery({ 
            queryKey: ['connectionStatusQueryKey'], 
            queryFn: () => getJSON('/serverConnected')
        });
    function renderStatus () {
        if (isLoading) return <div>Trying to reach server...</div>;
        else if (error) return <div>An error occurred: {error.message}</div>;
        else if (actualOnline) return <div>Online</div>;
        else return <div>Offline</div>;
    };

    return (
        <>
            {renderStatus()}
        </>
    );
}

function OfflinePwa (){

    function renderStatus () {
        return <div>Offline</div>;
    };

    return (
        <>
            {renderStatus()}
        </>
        
    );
}

export default PwaController;