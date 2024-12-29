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

const putInCache = async (request, response) => {
    const cache = await caches.open("v1");
    await cache.put(request, response);
  };

async function requestNetwork(request) {
    const responseFromNetwork = await fetch(request);
    // response may be used only once
    // we need to save clone to put one copy in cache
    // and serve second one
    putInCache(request, responseFromNetwork.clone());
    console.log("Response from network is:", responseFromNetwork);
    return responseFromNetwork;
}

async function requestCache(request) {
    const responseFromCache = await caches.match(request);
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

const cacheFirst = async ({ request, fallbackUrl }) => {
    // copy of requestCache() 
    const responseFromCache = await caches.match(request);
    if (responseFromCache) {
        console.log("Found response in cache:", responseFromCache);
        return responseFromCache;
    }
    try {
        return requestNetwork(request);
    } catch (error) {
        return requestsFailed (error, fallbackUrl);
    }
};

const networkFirst = async ({ request, fallbackUrl }) => {
    // copy of requestNetwork()
    const responseFromNetwork = await fetch(request);
    if (responseFromNetwork.ok) {
        putInCache(request, responseFromNetwork.clone());
        //console.log("Response from network is:", responseFromNetwork);
        return responseFromNetwork;
    }
    try {
        return requestCache(request);
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
    event.respondWith(
        networkFirst({
            request: event.request,
            fallbackUrl: "/src/color.css",
        }),
    );
});
