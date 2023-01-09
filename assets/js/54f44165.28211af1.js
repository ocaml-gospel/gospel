"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[152],{3905:function(e,t,n){n.d(t,{Zo:function(){return c},kt:function(){return d}});var r=n(7294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function a(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?a(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):a(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,r,o=function(e,t){if(null==e)return{};var n,r,o={},a=Object.keys(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(r=0;r<a.length;r++)n=a[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var s=r.createContext({}),p=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},c=function(e){var t=p(e.components);return r.createElement(s.Provider,{value:t},e.children)},u="mdxType",f={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,o=e.mdxType,a=e.originalType,s=e.parentName,c=i(e,["components","mdxType","originalType","parentName"]),u=p(n),m=o,d=u["".concat(s,".").concat(m)]||u[m]||f[m]||a;return n?r.createElement(d,l(l({ref:t},c),{},{components:n})):r.createElement(d,l({ref:t},c))}));function d(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=n.length,l=new Array(a);l[0]=m;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i[u]="string"==typeof e?e:o,l[1]=i;for(var p=2;p<a;p++)l[p]=n[p];return r.createElement.apply(null,l)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},681:function(e,t,n){n.r(t),n.d(t,{assets:function(){return s},contentTitle:function(){return l},default:function(){return u},frontMatter:function(){return a},metadata:function(){return i},toc:function(){return p}});var r=n(7462),o=(n(7294),n(3905));const a={sidebar_position:1},l="Installing Gospel",i={unversionedId:"getting-started/installation",id:"getting-started/installation",title:"Installing Gospel",description:"Please make sure that you already have a decently recent version of ocaml and",source:"@site/docs/getting-started/installation.md",sourceDirName:"getting-started",slug:"/getting-started/installation",permalink:"/gospel/getting-started/installation",draft:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_position:1},sidebar:"tutorialSidebar",previous:{title:"Welcome!",permalink:"/gospel/"},next:{title:"Your first specification",permalink:"/gospel/getting-started/first-spec"}},s={},p=[],c={toc:p};function u(e){let{components:t,...n}=e;return(0,o.kt)("wrapper",(0,r.Z)({},c,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"installing-gospel"},"Installing Gospel"),(0,o.kt)("p",null,"Please make sure that you already have a decently recent version of ",(0,o.kt)("inlineCode",{parentName:"p"},"ocaml")," and\n",(0,o.kt)("inlineCode",{parentName:"p"},"opam")," installed. Gospel requires the following versions:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"OCaml 4.09.0 or newer"),(0,o.kt)("li",{parentName:"ul"},"Opam 2.0 or newer")),(0,o.kt)("p",null,"Please visit ",(0,o.kt)("a",{parentName:"p",href:"https://ocaml.org/learn/tutorials/up_and_running.html"},"ocaml.org"),"\nfor instructions on how to get started with your OCaml environment."),(0,o.kt)("p",null,"Gospel is available on Opam repositories. Installing it is straightforward:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"$ opam install gospel\n")),(0,o.kt)("p",null,"This will install the ",(0,o.kt)("inlineCode",{parentName:"p"},"gospel")," tool binary, as well as the developer library\nif you wish to build your software on top of Gospel. You may check the\ninstallation with:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"$ gospel --version\ngospel version 0.1.0\n")))}u.isMDXComponent=!0}}]);