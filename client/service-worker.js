const idb_source = "https://cdn.jsdelivr.net/npm/idb@8/build/umd.js";
importScripts(idb_source);

// --- Install & Activate --- //

addEventListener("install", (event) => {
    event.addRoutes({
      condition: {
        urlPattern: "/src/*",
        runningStatus: "not-running",
      },
      source: "network",
    });
    console.log("Worker installed")
});

self.addEventListener('activate', event => {
  console.log('Service worker activated');
});

// --- Start db and worker --- //

const todoDBName = 'pending-todos';
const todoDBVersion = 1;
createTodoDB();

const SW_VESRION = "v1.1_";
const STATIC_CACHE = SW_VESRION + "static"
const SERVER_STATE_CACHE = SW_VESRION + "server_state";
  
self.addEventListener("fetch", (event) => {
    if (event.request.method !== "GET") {
        console.log('Not GET request. No Service-worker effect');
        return;
    };
    if (requestIsForStaticContent(event.request)) {
        //console.log("Fetching static content from cache: ", event.request)
        event.respondWith(
            cacheFirst({
                cacheName: STATIC_CACHE,
                request: event.request
            }),
        )
    }
    else {
        event.respondWith(
            networkFirst({
                cacheName: SERVER_STATE_CACHE,
                request: event.request
            }),
        );
    }
});

// --- Local Caching --- //

const putInCache = async (cacheName, request, response) => {
    const cache = await caches.open(cacheName);
    await cache.put(request, response);
};

async function requestNetwork(cacheName, request) {
    const responseFromNetwork = await fetch(request);
    // clone because response may be used only once
    putInCache(cacheName, request, responseFromNetwork.clone());
    console.log("Response from network:", responseFromNetwork);
    return responseFromNetwork;
}

async function requestCache(cacheName, request) {
    const responseFromCache = caches.match(request);
    return responseFromCache;
}

async function requestsFailed (error) {
    console.error(error);
    return new Response("Network error happened", {
        status: 408,
        headers: { "Content-Type": "text/plain" },
    });
}

const cacheFirst = async ({cacheName,  request }) => {
    const responseFromCache = await caches.match(request);
    if (responseFromCache && responseFromCache.ok) {
        return responseFromCache;
    }
    try {
        return requestNetwork(cacheName, request);
    } catch (error) {
        return requestsFailed (error);
    }
};

const networkFirst = async ({ cacheName, request }) => {
    const responseFromNetwork = await fetch(request);
    if (responseFromNetwork.ok) {
        putInCache(cacheName, request, responseFromNetwork.clone());
        return responseFromNetwork;
    }
    try {
        return requestCache(cacheName, request);
    } catch (error) {
        return requestsFailed (error);
    }
};

function requestIsForStaticContent(request) {
    const dev_client_host = ":5173/";
    const prod_client_host = ":5050/";

    const patterns = [idb_source, dev_client_host, prod_client_host];

    function matchFound (previousMatch, pattern) {
        if (previousMatch || request.url.includes(pattern)) {return true}
        else return false;
    }
    const urlMatchesAnyPattern = patterns.reduce(matchFound, false);

    return urlMatchesAnyPattern; // Bool return
}

// --- IndexedDB for offline work --- //

async function openTodoDB () {
    return await openDB(todoDBName, todoDBVersion);
}

async function createTodoDB () {
    const dbPromise = await idb.openDB(todoDBName, todoDBVersion, {
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