/**
 * Welcome to your Workbox-powered service worker!
 *
 * You'll need to register this file in your web app and you should
 * disable HTTP caching for this file too.
 * See https://goo.gl/nhQhGp
 *
 * The rest of the code is auto-generated. Please don't update this file
 * directly; instead, make changes to your Workbox build configuration
 * and re-run your build process.
 * See https://goo.gl/2aRDsh
 */

importScripts("workbox-v3.6.3/workbox-sw.js");
workbox.setConfig({modulePathPrefix: "workbox-v3.6.3"});

workbox.core.setCacheNameDetails({prefix: "gatsby-plugin-offline"});

workbox.skipWaiting();
workbox.clientsClaim();

/**
 * The workboxSW.precacheAndRoute() method efficiently caches and responds to
 * requests for URLs in the manifest.
 * See https://goo.gl/S9QRab
 */
self.__precacheManifest = [
  {
    "url": "webpack-runtime-f25b802dfe48024c56c3.js"
  },
  {
    "url": "app-760c462b0a728fc001ac.js"
  },
  {
    "url": "component---node-modules-gatsby-plugin-offline-app-shell-js-70fa0028ee7496e8897a.js"
  },
  {
    "url": "index.html",
    "revision": "9d143729d39d99e6fc14e20e8ebb9a47"
  },
  {
    "url": "offline-plugin-app-shell-fallback/index.html",
    "revision": "3a82c1e7112c9c11fb966a5b739fafc1"
  },
  {
    "url": "1.714e789c58edc4d97d4f.css"
  },
  {
    "url": "1-a86c05ed8d88fa3e9780.js"
  },
  {
    "url": "component---src-templates-index-js-4701c8e2fbaebcc1776b.js"
  },
  {
    "url": "0-f1f0de21e635ea99a0cc.js"
  },
  {
    "url": "static/d/585/path---index-6a9-Kp8g88TIdZg1pof4PD3S5XDgokg.json",
    "revision": "5672ec191b2bdbf35adb40998a780016"
  },
  {
    "url": "component---src-pages-404-js-73aa4ee436ca34094f3e.js"
  },
  {
    "url": "static/d/164/path---404-html-516-62a-NZuapzHg3X9TaN1iIixfv1W23E.json",
    "revision": "c2508676a2f33ea9f1f0bf472997f9a0"
  },
  {
    "url": "static/d/520/path---offline-plugin-app-shell-fallback-a-30-c5a-NZuapzHg3X9TaN1iIixfv1W23E.json",
    "revision": "c2508676a2f33ea9f1f0bf472997f9a0"
  },
  {
    "url": "manifest.webmanifest",
    "revision": "44fe3d35b32791e5923b42ffc58bb7d0"
  }
].concat(self.__precacheManifest || []);
workbox.precaching.suppressWarnings();
workbox.precaching.precacheAndRoute(self.__precacheManifest, {});

workbox.routing.registerNavigationRoute("/./tiagoblog/offline-plugin-app-shell-fallback/index.html", {
  whitelist: [/^[^?]*([^.?]{5}|\.html)(\?.*)?$/],
  blacklist: [/\?(.+&)?no-cache=1$/],
});

workbox.routing.registerRoute(/\.(?:png|jpg|jpeg|webp|svg|gif|tiff|js|woff|woff2|json|css)$/, workbox.strategies.staleWhileRevalidate(), 'GET');
workbox.routing.registerRoute(/^https:/, workbox.strategies.networkFirst(), 'GET');
"use strict";

/* global workbox */
self.addEventListener("message", function (event) {
  var api = event.data.api;

  if (api === "gatsby-runtime-cache") {
    var resources = event.data.resources;
    var cacheName = workbox.core.cacheNames.runtime;
    event.waitUntil(caches.open(cacheName).then(function (cache) {
      return Promise.all(resources.map(function (resource) {
        return cache.add(resource).catch(function (e) {
          // ignore TypeErrors - these are usually due to
          // external resources which don't allow CORS
          if (!(e instanceof TypeError)) throw e;
        });
      }));
    }));
  }
});