import {} from 'idb';

import { getTodos } from './networking';
import { createTodoDB, cacheFailedTodo, getUnsyncedTodos, flushDbToServer } from './databasing';


const mutationRequest = async (todo, request) => {
    const attempt = await 
        fetch(request)
        .then(response => {
            if (response.ok) {
                return response;
            } else return newErrorResponse();
        })
        .catch(error => {
            console.log("Not OK response from network. Caching request.")
            cacheRequest(request);
            return requestsFailed (error);
        })
    return attempt; 
};

