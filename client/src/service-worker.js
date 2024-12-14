importScripts('https://storage.googleapis.com/workbox-cdn/releases/7.3.0/workbox-sw.js');

workbox.routing.registerRoute(
    ({request}) => request.destination === "image", 
    new workbox.strategies.CacheFirst()
);

workbox.routing.registerRoute(
    ({request}) => request.destination === "script", 
    new workbox.strategies.NetworkFirst()
);