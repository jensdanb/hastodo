
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

// --- Start worker --- //

const SW_VESRION = "v1.1_";
const STATIC_CACHE = SW_VESRION + "static"
const SERVER_STATE_CACHE = SW_VESRION + "server_state";
  
self.addEventListener("fetch", (event) => {
    if (event.request.method !== "GET") {
        return;
    }
    else if (requestIsForStaticContent(event.request)) {
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

const newErrorResponse = () => {
    return new Response("Network error happened", {
        status: 408,
        headers: { "Content-Type": "text/plain" },
    });
}

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
    return newErrorResponse();
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
    const responseFromNetwork = await 
        fetch(request)
        .catch((error) => {console.error('Error: ', error)}
    );
    if (responseFromNetwork.ok) {
        putInCache(cacheName, request, responseFromNetwork.clone());
        return responseFromNetwork.json();
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
    const patterns = [dev_client_host, prod_client_host];

    function matchFound (previousMatch, pattern) {
        if (previousMatch || request.url.includes(pattern)) {return true}
        else return false;
    }
    const urlMatchesAnyPattern = patterns.reduce(matchFound, false);

    return urlMatchesAnyPattern; // Bool return
}

// --- IndexedDB for offline work --- //
