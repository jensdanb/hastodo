import {openDB} from 'idb';

// --- Initialize ---

const todoDBName = 'pending-todos';
const todoDBVersion = 2;

function startDB () {
    if (!('indexedDB' in window)) {
      console.log("This browser doesn't support IndexedDB");
      return;
    } else {
        createTodoDB();
    }
  }

async function openTodoDB () {
    return await openDB(todoDBName, todoDBVersion);
}

async function createTodoDB () {
    const dbPromise = await openDB(todoDBName, todoDBVersion, {
        upgrade (db, oldVersion) {
            const createStores = () => {
                const postsStore = db.createObjectStore('posts', { autoIncrement: true });
                postsStore.createIndex('posts', 'id', {unique: true});
            
                const putsStore = db.createObjectStore('puts', { autoIncrement: true });
                putsStore.createIndex('puts', 'id', {unique: true});
            };
            switch (oldVersion) {
                case 0: 
                    createStores();

                case 1: 
                    console.log('Version 1 found. Delete and start from scratch.')
                    db.deleteObjectStore('posts');
                    db.deleteObjectStore('puts');
                    createStores();
            }
        }
    });
}

// --- Use ---

async function cacheFailedTodo(failedMethod, todo) {
    const db = await openDB(todoDBName, todoDBVersion);
    await db.add(failedMethod, todo);
}

async function getUnsyncedTodos() {
    const db = await openDB(todoDBName, todoDBVersion);
    return await db.getAll('posts')
}

async function networkTransaction(networkAction, dbAction) {
    const db = await openDB(todoDBName, todoDBVersion);
    const tx = db.transaction('posts', 'readwrite');
}

// --- Junk --- 


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

export {startDB, openTodoDB, cacheFailedTodo, getUnsyncedTodos}
