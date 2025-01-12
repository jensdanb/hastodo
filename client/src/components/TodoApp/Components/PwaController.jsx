import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import 'idb';

import { getJSON, hsUrl } from '../../../services/networking';



function PwaController () {
    const [intendedOnline, setIntendedOnline] = useState(false);

    return (
        <>
            {intendedOnline ? <OnlinePwa/> : <OfflinePwa/>}
            <button 
                type="button" 
                className="btn toggle-btn" 
                aria-pressed="true"
                onMouseDown={() => setIntendedOnline(false)}>
                <span>Go offline </span>
            </button>
        </>)
};

function OnlinePwa (){
    const {data: actualOnline, isLoading, error} = useQuery({ 
            queryKey: ['connectionStatusQueryKey'], 
            queryFn: () => getJSON('/serverConnected')
        });
    function renderStatus () {
        if (isLoading) return <div>Trying to reach server...</div>;
        else if (error) return <div>Failed to reach server</div>;
        else if (actualOnline) return <div>Online</div>;
        else return <div>Offline</div>;
    };

    return (
        <>
            {renderStatus()}
        </>
    );
};

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