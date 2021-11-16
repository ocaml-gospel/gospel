"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[14],{3905:function(e,n,t){t.d(n,{Zo:function(){return p},kt:function(){return m}});var a=t(7294);function i(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function o(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function r(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?o(Object(t),!0).forEach((function(n){i(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):o(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function c(e,n){if(null==e)return{};var t,a,i=function(e,n){if(null==e)return{};var t,a,i={},o=Object.keys(e);for(a=0;a<o.length;a++)t=o[a],n.indexOf(t)>=0||(i[t]=e[t]);return i}(e,n);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)t=o[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(i[t]=e[t])}return i}var l=a.createContext({}),s=function(e){var n=a.useContext(l),t=n;return e&&(t="function"==typeof e?e(n):r(r({},n),e)),t},p=function(e){var n=s(e.components);return a.createElement(l.Provider,{value:n},e.children)},d={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},u=a.forwardRef((function(e,n){var t=e.components,i=e.mdxType,o=e.originalType,l=e.parentName,p=c(e,["components","mdxType","originalType","parentName"]),u=s(t),m=i,f=u["".concat(l,".").concat(m)]||u[m]||d[m]||o;return t?a.createElement(f,r(r({ref:n},p),{},{components:t})):a.createElement(f,r({ref:n},p))}));function m(e,n){var t=arguments,i=n&&n.mdxType;if("string"==typeof e||i){var o=t.length,r=new Array(o);r[0]=u;var c={};for(var l in n)hasOwnProperty.call(n,l)&&(c[l]=n[l]);c.originalType=e,c.mdxType="string"==typeof e?e:i,r[1]=c;for(var s=2;s<o;s++)r[s]=t[s];return a.createElement.apply(null,r)}return a.createElement.apply(null,t)}u.displayName="MDXCreateElement"},2628:function(e,n,t){t.r(n),t.d(n,{frontMatter:function(){return c},contentTitle:function(){return l},metadata:function(){return s},toc:function(){return p},default:function(){return u}});var a=t(7462),i=t(3366),o=(t(7294),t(3905)),r=["components"],c={sidebar_position:7},l="Logical declarations",s={unversionedId:"language/logical",id:"language/logical",isDocsHomePage:!1,title:"Logical declarations",description:"Functions and Predicates",source:"@site/docs/language/logical.md",sourceDirName:"language",slug:"/language/logical",permalink:"/gospel/language/logical",tags:[],version:"current",sidebarPosition:7,frontMatter:{sidebar_position:7},sidebar:"tutorialSidebar",previous:{title:"Function contracts",permalink:"/gospel/language/function-contracts"},next:{title:"Tips and good practices",permalink:"/gospel/good-practices"}},p=[{value:"Functions and Predicates",id:"functions-and-predicates",children:[],level:2},{value:"Uninterpreted symbols and Axioms",id:"uninterpreted-symbols-and-axioms",children:[],level:2},{value:"Logical function contracts",id:"logical-function-contracts",children:[],level:2},{value:"Termination arguments",id:"termination-arguments",children:[],level:2}],d={toc:p};function u(e){var n=e.components,t=(0,i.Z)(e,r);return(0,o.kt)("wrapper",(0,a.Z)({},d,t,{components:n,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"logical-declarations"},"Logical declarations"),(0,o.kt)("h2",{id:"functions-and-predicates"},"Functions and Predicates"),(0,o.kt)("p",null,"It is often convenient to introduce shortcuts for terms and formulas to avoid\nrepetitions. ",(0,o.kt)("em",{parentName:"p"},"Predicates")," let you write named formulae definitions in Gospel\ncomments. Here is a typical example:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml"},"(*@ predicate is_sorted (a: int array) =\n      forall i j. 0 <= i <= j < Array.length a\n                  -> a.(i) <= a.(j) *)\n")),(0,o.kt)("p",null,"We can then reuse the predicate ",(0,o.kt)("inlineCode",{parentName:"p"},"is_sorted")," inside any Gospel annotations such\nas function contracts:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml"},"val merge: int array -> int array -> int array\n(*@ c = merge a b\n    requires is_sorted a\n    requires is_sorted b\n    ensures is_sorted c *)\n")),(0,o.kt)("p",null,"Similarly, one can define a shortcut for terms using Gospel's ",(0,o.kt)("em",{parentName:"p"},"functions"),"."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml"},"(*@ function powm (x y m: integer) : integer = mod (pow x y) m *)\n")),(0,o.kt)("p",null,"Both predicate definitions and function definitions may be\nrecursive. A recursive definition requires the ",(0,o.kt)("inlineCode",{parentName:"p"},"rec")," keyword like in OCaml:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml"},"(*@ predicate rec is_sorted_list (l: int list) = match l with\n      | [] | _ :: [] -> true\n      | h :: (y :: _ as t) -> h <= y /\\ is_sorted_list t *)\n")),(0,o.kt)("h2",{id:"uninterpreted-symbols-and-axioms"},"Uninterpreted symbols and Axioms"),(0,o.kt)("h2",{id:"logical-function-contracts"},"Logical function contracts"),(0,o.kt)("p",null,"Similarly to OCaml functions, contracts can be added to logical declarations."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{3}","{3}":!0},"(*@ function rec fibonacci (n: integer) : integer =\n      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)\n(*@ requires n >= 0 *)\n")),(0,o.kt)("p",null,"Such a contract does not prevent you from calling ",(0,o.kt)("inlineCode",{parentName:"p"},"fibonacci")," on negative\nintegers. For instance, ",(0,o.kt)("inlineCode",{parentName:"p"},"fibonacci (-1)")," is a valid Gospel term. However, we\nknow nothing about its value: the definition of ",(0,o.kt)("inlineCode",{parentName:"p"},"fibonacci")," holds only when its\nprecondition is true."),(0,o.kt)("p",null,"The above is equivalent to an uninterpreted function together with an axiom, as\nfollows:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml"},"(*@ function fibonacci (n: integer) : integer *)\n\n(*@ axiom fibonacci_def : forall n. n >= 0 ->\n      fibonacci n =\n        if n <= 1 then n\n        else fibonacci (n-2) + fibonacci (n-1) *)\n")),(0,o.kt)("p",null,"Logical symbols can also come with post-conditions. For instance, we can assert\nthat Fibonacci numbers are non-negative:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{4}","{4}":!0},"(*@ function rec fibonacci (n: integer) : integer =\n      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)\n(*@ requires n >= 0\n    ensures result >= 0 *)\n")),(0,o.kt)("div",{className:"admonition admonition-info alert alert--info"},(0,o.kt)("div",{parentName:"div",className:"admonition-heading"},(0,o.kt)("h5",{parentName:"div"},(0,o.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,o.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"14",height:"16",viewBox:"0 0 14 16"},(0,o.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M7 2.3c3.14 0 5.7 2.56 5.7 5.7s-2.56 5.7-5.7 5.7A5.71 5.71 0 0 1 1.3 8c0-3.14 2.56-5.7 5.7-5.7zM7 1C3.14 1 0 4.14 0 8s3.14 7 7 7 7-3.14 7-7-3.14-7-7-7zm1 3H6v5h2V4zm0 6H6v2h2v-2z"}))),"info")),(0,o.kt)("div",{parentName:"div",className:"admonition-content"},(0,o.kt)("p",{parentName:"div"},"Note that as opposed to OCaml function contracts, logical function contracts do\nnot have a header. Consequently, a variable called ",(0,o.kt)("inlineCode",{parentName:"p"},"result")," is automatically\nintroduced in the context by Gospel to refer to the value returned by the\nfunction in a post-condition."))),(0,o.kt)("p",null,"The post-condition of ",(0,o.kt)("inlineCode",{parentName:"p"},"fibonacci")," is equivalent to adding an axiom along with an\nuninterpreted counterpart."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml"},"(*@ axiom fibonacci_post : forall n. n >= 0 -> fibonacci n >= 0 *)\n")),(0,o.kt)("p",null,"Note that the post-condition holds only when the pre-condition holds."),(0,o.kt)("div",{className:"admonition admonition-danger alert alert--danger"},(0,o.kt)("div",{parentName:"div",className:"admonition-heading"},(0,o.kt)("h5",{parentName:"div"},(0,o.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,o.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"12",height:"16",viewBox:"0 0 12 16"},(0,o.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M5.05.31c.81 2.17.41 3.38-.52 4.31C3.55 5.67 1.98 6.45.9 7.98c-1.45 2.05-1.7 6.53 3.53 7.7-2.2-1.16-2.67-4.52-.3-6.61-.61 2.03.53 3.33 1.94 2.86 1.39-.47 2.3.53 2.27 1.67-.02.78-.31 1.44-1.13 1.81 3.42-.59 4.78-3.42 4.78-5.56 0-2.84-2.53-3.22-1.25-5.61-1.52.13-2.03 1.13-1.89 2.75.09 1.08-1.02 1.8-1.86 1.33-.67-.41-.66-1.19-.06-1.78C8.18 5.31 8.68 2.45 5.05.32L5.03.3l.02.01z"}))),"danger")),(0,o.kt)("div",{parentName:"div",className:"admonition-content"},(0,o.kt)("p",{parentName:"div"},"Gospel does not perform any verification beyond type-checking. If you wish to\nverify that the definition indeed complies with its contract, you need to use an\nexternal tool such as ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/ocaml-gospel/why3gospel"},"Why3Gospel"),"."))),(0,o.kt)("h2",{id:"termination-arguments"},"Termination arguments"),(0,o.kt)("p",null,"Using recursive definitions in the logical domain can introduce inconsistencies.\nFor instance, consider the following recursive function:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml"},"(*@ function rec f (n: integer): integer = f n + 1 *)\n")),(0,o.kt)("p",null,"As explained above, it is perfectly fine to mention ",(0,o.kt)("inlineCode",{parentName:"p"},"f 0")," in a formula. Although\nwe do not know its value, we know that ",(0,o.kt)("inlineCode",{parentName:"p"},"f 0 = f 0 + 1"),", thus ",(0,o.kt)("inlineCode",{parentName:"p"},"0 = 1"),", which is\nobviously inconsistent."),(0,o.kt)("p",null,"In order to prevent this, it is a good practice to provide a termination\nargument for each recursive definition. Gospel provides one way of doing this\nvia ",(0,o.kt)("em",{parentName:"p"},"variants"),"."),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-ocaml",metastring:"{4}","{4}":!0},"(*@ function rec fibonacci (n: integer) : integer =\n      if n <= 1 then n else fibonacci (n-2) + fibonacci (n-1) *)\n(*@ requires n >= 0\n    variant n *)\n")),(0,o.kt)("div",{className:"admonition admonition-danger alert alert--danger"},(0,o.kt)("div",{parentName:"div",className:"admonition-heading"},(0,o.kt)("h5",{parentName:"div"},(0,o.kt)("span",{parentName:"h5",className:"admonition-icon"},(0,o.kt)("svg",{parentName:"span",xmlns:"http://www.w3.org/2000/svg",width:"12",height:"16",viewBox:"0 0 12 16"},(0,o.kt)("path",{parentName:"svg",fillRule:"evenodd",d:"M5.05.31c.81 2.17.41 3.38-.52 4.31C3.55 5.67 1.98 6.45.9 7.98c-1.45 2.05-1.7 6.53 3.53 7.7-2.2-1.16-2.67-4.52-.3-6.61-.61 2.03.53 3.33 1.94 2.86 1.39-.47 2.3.53 2.27 1.67-.02.78-.31 1.44-1.13 1.81 3.42-.59 4.78-3.42 4.78-5.56 0-2.84-2.53-3.22-1.25-5.61-1.52.13-2.03 1.13-1.89 2.75.09 1.08-1.02 1.8-1.86 1.33-.67-.41-.66-1.19-.06-1.78C8.18 5.31 8.68 2.45 5.05.32L5.03.3l.02.01z"}))),"danger")),(0,o.kt)("div",{parentName:"div",className:"admonition-content"},(0,o.kt)("p",{parentName:"div"},"Similarly to contracts, Gospel does not perform any verification that the\nvariant indeed ensures the termination. It is up to an external tool to help you\nverify this."))))}u.isMDXComponent=!0}}]);