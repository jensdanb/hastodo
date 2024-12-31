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
const EDIT_QUEUE_CHACE = SW_VESRION + "queue";


const putInCache = async (cacheName, request, response) => {
    const cache = await caches.open(cacheName);
    await cache.put(request, response);
};

async function requestNetwork(cacheName, request) {
    const responseFromNetwork = await fetch(request);
    // response may be used only once
    // we need to save clone to put one copy in cache
    // and serve second one
    putInCache(cacheName, request, responseFromNetwork.clone());
    console.log("Response from network is:", responseFromNetwork);
    return responseFromNetwork;
}

async function requestCache(cacheName, request) {
    const cache = await caches.open(cacheName);
    const responseFromCache = await cache.match(request);
    console.log("Found response in cache:", responseFromCache);
    return responseFromCache;

}

async function requestsFailed (error, fallbackUrl) {
    const fallbackResponse = await caches.match(fallbackUrl);
    if (fallbackResponse) {
        return fallbackResponse;
    }
    // when even the fallback response is not available,
    // there is nothing we can do, but we must always
    // return a Response object
    console.error("Fetching failed, both cache and network");
    return new Response("Network error happened", {
        status: 408,
        headers: { "Content-Type": "text/plain" },
    });
}

const cacheFirst = async ({cacheName,  request, fallbackUrl }) => {
    // copy of requestCache() 
    const responseFromCache = await caches.match(request);
    if (responseFromCache) {
        return responseFromCache;
    }
    try {
        return requestNetwork(cacheName, request);
    } catch (error) {
        return requestsFailed (error, fallbackUrl);
    }
};

const networkFirst = async ({ cacheName, request, fallbackUrl }) => {
    // copy of requestNetwork()
    const responseFromNetwork = await fetch(request);
    if (responseFromNetwork.ok) {
        putInCache(cacheName, request, responseFromNetwork.clone());
        return responseFromNetwork;
    }
    try {
        return requestCache(cacheName, request);
    } catch (error) {
        return requestsFailed (error, fallbackUrl);
    }
};
  
self.addEventListener("fetch", (event) => {
    //console.log("Caught event" + event.request.url)
    if (event.request.method !== "GET") {
        console.log('Not GET request. No Service-worker effect');
        return;
    };
    if (requestIsForStaticContent(event.request)) {
        event.respondWith(
            cacheFirst({
                cacheName: STATIC_CACHE,
                request: event.request,
                fallbackUrl: "/src/color.css",
            }),
        )
    }
    else {
        event.respondWith(
            networkFirst({
                cacheName: SERVER_STATE_CACHE,
                request: event.request,
                fallbackUrl: "/src/color.css",
            }),
        );
    }
});

function requestIsForStaticContent(request) {
    const pattern = ":5173/"
    if (request.url.includes(pattern)) {return true;}
    else return false;
}