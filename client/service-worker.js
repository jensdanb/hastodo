importScripts('https://storage.googleapis.com/workbox-cdn/releases/7.3.0/workbox-sw.js');

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


/*
workbox.routing.registerRoute(
    ({request}) => request.destination === "image", 
    new workbox.strategies.CacheFirst()
);

workbox.routing.registerRoute(
    ({request}) => request.destination === "script", 
    new workbox.strategies.NetworkFirst()
);
*/

const SW_VESRION = "v1.1_";
const STATIC_CACHE = SW_VESRION + "static"
const SERVER_STATE_CACHE = SW_VESRION + "server_state";

// POST/PUT queue must be in indexedDB. Import it.


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
  
self.addEventListener("fetch", (event) => {
    if (event.request.method !== "GET") {
        console.log('Not GET request. No Service-worker effect');
        return;
    };
    if (requestIsForStaticContent(event.request)) {
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

function requestIsForStaticContent(request) {
    const pattern = ":5173/"
    if (request.url.includes(pattern)) {return true;}
    else return false;
}