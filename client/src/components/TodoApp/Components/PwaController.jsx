import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import 'idb';

import { getJSON, hsUrl } from '../../../services/networking';



function PwaController (props) {

    return (
        <>
            {props.intendedOnline ? <OnlinePwa actualOnline={props.actualOnline}/> : <OfflinePwa/>}
            <button 
                type="button" 
                className="btn toggle-btn" 
                aria-pressed="true"
                onMouseDown={props.toggleOnline}>
                <span>Toggle online </span>
            </button>
        </>)
};

function OnlinePwa (props){
    
    function renderStatus () {
        if (props.actualOnline) return <div>Online</div>;
        else return <div>Connection failed</div>;
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