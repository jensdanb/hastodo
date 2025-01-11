import {openDB} from 'idb';

const todoDBName = 'pending-todos';
const todoDBVersion = 1;
createTodoDB();

async function openTodoDB () {
    return await idb.openDB(todoDBName, todoDBVersion);
}

async function createTodoDB () {
    const dbPromise = await idb.openDB(todoDBName, todoDBVersion, {
        upgrade (db, oldVersion) {
            const createStores = () => {
                const postsStore = db.createObjectStore('posts', { autoIncrement: true });
                postsStore.createIndex('id', 'id', {unique: true});
            
                const putsStore = db.createObjectStore('puts', { autoIncrement: true });
                putsStore.createIndex('id', 'id', {unique: true});

                const requestStore = db.createObjectStore('requests', { autoIncrement: true });
                requestStore.createIndex('id', 'id', {unique: true});
                requestStore.createIndex('method', 'method', {unique: false});
            };
            switch (oldVersion) {
                case 0: 
                    createStores();
                /*
                case 1: 
                    console.log('Version 1 found. Delete and start from scratch.')
                    db.deleteObjectStore('posts');
                    db.deleteObjectStore('puts');
                    createStores();
                */
            }
        }
    });
}

async function cacheFailedTodo(failedMethod, todo) {
    const db = await openTodoDB();
    await db.add(failedMethod, todo);
}

async function cacheRequest(request) {
    const db = await openTodoDB();
    const headerArray = Object.from(request.headers.entries());
    const requestBody = request.body;
    const requestData = {
        url: request.url, 
        method: request.method, 
        headers: headerArray,
        body: requestBody
    };
    console.log(requestData);
    await db.add("requests", requestData);
}

async function getUnsyncedTodos() {
    const db = await openTodoDB();
    return await db.getAll('posts')
}

async function networkTransaction(networkAction, dbAction) {
    const db = await openTodoDB();
    const tx = db.transaction('posts', 'readwrite');
}

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

export default {createTodoDB, cacheFailedTodo, getUnsyncedTodos, flushDbToServer};

// --- Junk --- 

/*
async function addItemToStore () {
    const db = await openDB('example-database', 1, {
        upgrade (db) {
            if (!db.objectStoreNames.contains('foods')) {
                db.createObjectStore('foods', {keyPath: 'name'})
            }
        }
    });

    const tx = db.transaction('foods', 'readwrite');
  
    await Promise.all([
        tx.store.add({
            name: 'sandwhich', 
            price: 4.99,
            timeCreated: new Date().getDate()
        }), 
        tx.store.add({
            name: 'ice cream', 
            price: 3.49, 
            timeCreated: new Date().getDate()
        }), 
        tx.done
    ]);
}

async function getFood(name) {
    const db = await openDB('example-database', 1);
    const food = await db.get('foods', name);
    return food;
}

*/