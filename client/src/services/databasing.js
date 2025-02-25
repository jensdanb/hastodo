import {openDB, deleteDB} from 'idb'

const todoDBName = 'todoDb';
const todoDBVersion = 1;
const todoListStoreName = 'todoList'

async function openTodoDB () {
    return await openDB(todoDBName, todoDBVersion);
};

function notEmpty (cacheResult) {
    if (Array.isArray(cacheResult) && cacheResult.length != 0) 
        return true
    else return false;
};

function createTodoStore (db) {
    const todoListStore = db.createObjectStore(todoListStoreName, { autoIncrement: true });
    todoListStore.createIndex('id', 'id', {unique: true});
};

async function createTodoDB () {
    const dbPromise = await openDB(todoDBName, todoDBVersion, {
        upgrade (db, oldVersion) {
            switch (oldVersion) {
                case 0: 
                    console.log('No DB found. Creating it.')
                    createTodoStore(db);
                /*
                case 1: 
                    console.log('Version 1 found. Delete and start from scratch.')
                    db.deleteObjectStore('posts');
                    db.deleteObjectStore('puts');
                    createTodoStore(db);
                */
            }
        }
    });
};

async function cacheFailedTodo (failedMethod, todo) {
    const db = await openTodoDB();
    await db.add(failedMethod, todo);
}

async function getUnsyncedTodos () {
    const db = await openTodoDB();
    return await db.getAll('posts')
};

async function dbGetTodoList () {
    const db = await openTodoDB();
    return await db.getAll(todoListStoreName)
};

async function cacheTodoList (todos) {
    const db = await openTodoDB();
    const tx = db.transaction(todoListStoreName, 'readwrite')
    await Promise.all([
        tx.store.clear(), 
        todos.forEach((todo) => tx.store.add(todo))
    ])
};

async function dbAddTodo(todo) {
    const db = await openTodoDB();
    await db.add(todoListStoreName, todo);
}

async function dbPutTodo (todo) {
    const db = await openTodoDB();
    const tx = db.transaction(todoListStoreName, 'readwrite');
    const store = tx.store;
    const primaryKey = await store.index('id').getKey(todo.id);
    if (primaryKey === undefined) {
        console.log('Item not found');
        return;
    }
    await store.put(todo, primaryKey);
    tx.done;
};

async function dbDelTodo (todoId) {
    const db = await openTodoDB();
    const tx = db.transaction(todoListStoreName, 'readwrite');
    const store = tx.store;
    const primaryKey = await store.index('id').getKey(todoId);
    if (primaryKey === undefined) {
        console.log('Item not found');
        return;
    }
    await store.delete(primaryKey);
    tx.done;
};

export { cacheTodoList, dbGetTodoList, createTodoDB, dbAddTodo, dbPutTodo, dbDelTodo };

// --- Junk --- 

/*
async function flushDbToServer (todoDBName, ) {
    const db = await openTodoDB();
    const tx = db.transaction('posts', 'readwrite');
    const postsInCache = tx.objectStore('posts');

    const unSyncedTodos = await postsInCache.getAll();
    if (notEmpty(unSyncedTodos)) {
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
*/

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