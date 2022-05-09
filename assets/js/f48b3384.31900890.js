"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[497],{3905:function(e,t,r){r.d(t,{Zo:function(){return c},kt:function(){return m}});var n=r(7294);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function i(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function a(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?i(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):i(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function l(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},i=Object.keys(e);for(n=0;n<i.length;n++)r=i[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(n=0;n<i.length;n++)r=i[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var s=n.createContext({}),p=function(e){var t=n.useContext(s),r=t;return e&&(r="function"==typeof e?e(t):a(a({},t),e)),r},c=function(e){var t=p(e.components);return n.createElement(s.Provider,{value:t},e.children)},u={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},f=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,i=e.originalType,s=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),f=p(r),m=o,h=f["".concat(s,".").concat(m)]||f[m]||u[m]||i;return r?n.createElement(h,a(a({ref:t},c),{},{components:r})):n.createElement(h,a({ref:t},c))}));function m(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var i=r.length,a=new Array(i);a[0]=f;var l={};for(var s in t)hasOwnProperty.call(t,s)&&(l[s]=t[s]);l.originalType=e,l.mdxType="string"==typeof e?e:o,a[1]=l;for(var p=2;p<i;p++)a[p]=r[p];return n.createElement.apply(null,a)}return n.createElement.apply(null,r)}f.displayName="MDXCreateElement"},46:function(e,t,r){r.r(t),r.d(t,{assets:function(){return c},contentTitle:function(){return s},default:function(){return m},frontMatter:function(){return l},metadata:function(){return p},toc:function(){return u}});var n=r(7462),o=r(3366),i=(r(7294),r(3905)),a=["components"],l={sidebar_position:3},s="Now what?",p={unversionedId:"getting-started/tools",id:"getting-started/tools",title:"Now what?",description:"You've written your first specification. Now what can you do with it?",source:"@site/docs/getting-started/tools.md",sourceDirName:"getting-started",slug:"/getting-started/tools",permalink:"/gospel/getting-started/tools",draft:!1,tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_position:3},sidebar:"tutorialSidebar",previous:{title:"Your first specification",permalink:"/gospel/getting-started/first-spec"},next:{title:"Going further",permalink:"/gospel/getting-started/going-further"}},c={},u=[{value:"Cameleer",id:"cameleer",level:2},{value:"Ortac",id:"ortac",level:2},{value:"Why3gospel",id:"why3gospel",level:2}],f={toc:u};function m(e){var t=e.components,r=(0,o.Z)(e,a);return(0,i.kt)("wrapper",(0,n.Z)({},f,r,{components:t,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"now-what"},"Now what?"),(0,i.kt)("p",null,"You've written your first specification. Now what can you do with it?"),(0,i.kt)("p",null,"Well, your specification alone is already helpful, as it completes the\ndocstring, which may be incomplete or ambiguous, leading to wrong\ninterpretations of your semantics, or wrong usage of your library."),(0,i.kt)("p",null,"But besides the ",(0,i.kt)("inlineCode",{parentName:"p"},"gospel")," binary, we also provide a developer API which lets\nother tools leverage these specifications to provide different features. Such\ntools already exist, and let you benefit from the specification to bring more\nguarantees to your programs."),(0,i.kt)("h2",{id:"cameleer"},"Cameleer"),(0,i.kt)("p",null,"Cameleer is a tool for deductive verification of OCaml code."),(0,i.kt)("p",null,"It extends Gospel to implementation files, where you may add logical annotations\nlike logical assertions, loop invariants, or termination arguments. The\nverification relies on the ",(0,i.kt)("a",{parentName:"p",href:"https://why3.lri.fr"},"Why3")," framework: ",(0,i.kt)("inlineCode",{parentName:"p"},"cameleer"),"\ntranslates the OCaml code into an equivalent WhyML program. It then lets you\nanalyse this program within the framework (and its IDE!) to prove the\nassertions via semi-automated techniques based on SMT provers."),(0,i.kt)("p",null,"For more information, please visit the project page ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/ocaml-gospel/cameleer"},"on\nGithub"),"."),(0,i.kt)("h2",{id:"ortac"},"Ortac"),(0,i.kt)("p",null,"Ortac is a runtime verification tool for OCaml programs."),(0,i.kt)("p",null,"It reads the Gospel annotations in the interfaces and generates code that\nautomatically checks them at runtime. It is implementation-agnostic and quite\nflexible: you may use it to trigger exceptions when violations occur, monitor\nyour program execution by logging unexpected events, or generate testing suites\nand fuzzers."),(0,i.kt)("p",null,"For more information, please visit the project page ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/ocaml-gospel/ortac"},"on\nGithub"),"."),(0,i.kt)("h2",{id:"why3gospel"},"Why3gospel"),(0,i.kt)("p",null,"Why3gospel is a ",(0,i.kt)("a",{parentName:"p",href:"https://why3.lri.fr"},"Why3")," plugin that lets you verify that a\nprogram proof refines the Gospel specifications before extracting it to OCaml."),(0,i.kt)("p",null,"It interfaces the Why3 framework with the Gospel specifications to ensure that\nthe former refines the latter, guaranteeing that OCaml programs extracted from\nproved WhyML indeed comply with their Gospel specification."),(0,i.kt)("p",null,"For more information, please visit the project page ",(0,i.kt)("a",{parentName:"p",href:"https://github.com/ocaml-gospel/why3gospel"},"on\nGithub"),"."))}m.isMDXComponent=!0}}]);