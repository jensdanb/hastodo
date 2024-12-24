import {openDB} from 'idb';

import {getUnsyncedTodos, openTodoDB} from './db-service';
import {postTodos} from './networking';

async function flushDbToServer(todoDBName, ) {
    const db = await openTodoDB();
    const tx = db.transaction('posts', 'readwrite');
    const postsInCache = tx.objectStore('posts');

    const unSyncedTodos = await postsInCache.getAll();
    if (Array.isArray(unSyncedTodos) && unSyncedTodos.length != 0) {
        console.log('Flushing');
        console.dir(unSyncedTodos);
        postTodos(unSyncedTodos)
            .then((response) => {
                console.dir(response);
                tx.done;
            });
        console.log(syncResponse);
    } 
    else console.dir('Nothing to upload: ' + unSyncedTodos.map(JSON.stringify));
};

export {flushDbToServer};