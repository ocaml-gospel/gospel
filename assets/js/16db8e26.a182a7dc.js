"use strict";(self.webpackChunkdocs=self.webpackChunkdocs||[]).push([[619],{3905:function(e,n,t){t.d(n,{Zo:function(){return c},kt:function(){return d}});var r=t(7294);function a(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);n&&(r=r.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,r)}return t}function p(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?i(Object(t),!0).forEach((function(n){a(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function o(e,n){if(null==e)return{};var t,r,a=function(e,n){if(null==e)return{};var t,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||(a[t]=e[t]);return a}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)t=i[r],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(a[t]=e[t])}return a}var l=r.createContext({}),s=function(e){var n=r.useContext(l),t=n;return e&&(t="function"==typeof e?e(n):p(p({},n),e)),t},c=function(e){var n=s(e.components);return r.createElement(l.Provider,{value:n},e.children)},u={inlineCode:"code",wrapper:function(e){var n=e.children;return r.createElement(r.Fragment,{},n)}},f=r.forwardRef((function(e,n){var t=e.components,a=e.mdxType,i=e.originalType,l=e.parentName,c=o(e,["components","mdxType","originalType","parentName"]),f=s(t),d=a,m=f["".concat(l,".").concat(d)]||f[d]||u[d]||i;return t?r.createElement(m,p(p({ref:n},c),{},{components:t})):r.createElement(m,p({ref:n},c))}));function d(e,n){var t=arguments,a=n&&n.mdxType;if("string"==typeof e||a){var i=t.length,p=new Array(i);p[0]=f;var o={};for(var l in n)hasOwnProperty.call(n,l)&&(o[l]=n[l]);o.originalType=e,o.mdxType="string"==typeof e?e:a,p[1]=o;for(var s=2;s<i;s++)p[s]=t[s];return r.createElement.apply(null,p)}return r.createElement.apply(null,t)}f.displayName="MDXCreateElement"},7985:function(e,n,t){t.r(n),t.d(n,{assets:function(){return c},contentTitle:function(){return l},default:function(){return d},frontMatter:function(){return o},metadata:function(){return s},toc:function(){return u}});var r=t(7462),a=t(3366),i=(t(7294),t(3905)),p=["components"],o={sidebar_position:3},l="Terms and Formulae",s={unversionedId:"language/formulae",id:"language/formulae",title:"Terms and Formulae",description:"Gospel features terms (e.g. x+1) and formulae (e.g. forall i. f i > 0). This",source:"@site/docs/language/formulae.md",sourceDirName:"language",slug:"/language/formulae",permalink:"/gospel/language/formulae",draft:!1,tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_position:3},sidebar:"tutorialSidebar",previous:{title:"Lexical conventions",permalink:"/gospel/language/lexical-conventions"},next:{title:"Type specifications",permalink:"/gospel/language/type-specifications"}},c={},u=[{value:"Type expressions",id:"type-expressions",level:2},{value:"Expressions",id:"expressions",level:2},{value:"Gospel-specific expressions",id:"gospel-specific-expressions",level:2}],f={toc:u};function d(e){var n=e.components,t=(0,a.Z)(e,p);return(0,i.kt)("wrapper",(0,r.Z)({},f,t,{components:n,mdxType:"MDXLayout"}),(0,i.kt)("h1",{id:"terms-and-formulae"},"Terms and Formulae"),(0,i.kt)("p",null,"Gospel features terms (e.g. ",(0,i.kt)("inlineCode",{parentName:"p"},"x+1"),") and formulae (e.g. ",(0,i.kt)("inlineCode",{parentName:"p"},"forall i. f i > 0"),"). This\ndistinction is made during type checking, and not at the syntax level. In\nthe following, ",(0,i.kt)("inlineCode",{parentName:"p"},"expr")," stands for a Gospel expression, be it a term or a formula."),(0,i.kt)("h2",{id:"type-expressions"},"Type expressions"),(0,i.kt)("p",null,"Type expressions follow the OCaml syntax."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ebnf"},'typexpr = lname\n        | "\'" lident\n        | "(" typexpr ")"\n        | typexpr lpath\n        | typexpr, { "," typexpr }\n        | (?"? lident ":")? typexpr "->" typexpr\n        | typexpr ("*" typexpr)+\nlpath = (upath ".")? lident\nupath = uident\n      | upath "." uident\n')),(0,i.kt)("h2",{id:"expressions"},"Expressions"),(0,i.kt)("p",null,"A large fragment of the OCaml syntax is reused for Gospel expressions."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ebnf"},'ocaml_expr = constant\n           | (upath ".")? ident\n           | "(" expr ")"\n           | "(" expr ("," expr)+ ")"\n           | expr "." "(" expr ")"\n           | expr infix_op expr\n           | prefix_op expr\n           | "not" expr\n           | expr expr+\n           | "if" expr "then" expr "else" expr\n           | "let" pattern "=" expr "in" expr\n           | "match" expr ("," expr)* "with" match_cases\n           | "fun" binders "->" expr\n           | expr ":" typexpr\n           | "{" fields "}"\n           | "{" expr "with fields "}"\n           | "[@...]" expr\nbinders = lident+ (":" typexpr)?\npattern = "_"\n        | lident\n        | uname pattern?\n        | "()"\n        | "(" pattern ")"\n        | pattern ("," pattern)+\n        | pattern "::" pattern\n        | pattern "as lident\n        | pattern "|" pattern\n        | "{" field_pattern_ (";" field_pattern)* "}"\nfield_pattern = lname "=" pattern\n              | lname\nmatch_cases = "|"? match_case ("|" match_case)*\nmatch_case = pattern "->" expr\nfields = field (";" field)*\nfield = lname "=" expr\n      | lname\nconstant = integer_literal\n         | real_literal\n         | "true" | "false"\n         | "()"\n         | "[]"\n')),(0,i.kt)("h2",{id:"gospel-specific-expressions"},"Gospel-specific expressions"),(0,i.kt)("p",null,"In addition, there is syntax that is specific to Gospel."),(0,i.kt)("pre",null,(0,i.kt)("code",{parentName:"pre",className:"language-ebnf"},'expr = ocaml_expr\n     | expr "/\\" expr\n     | expr "\\/" expr\n     | "old" expr\n     | quantifier binders ("," binders)* "." expr\n     | expr "[" expr "]"\n     | expr "[" expr "->" expr "]"\n     | expr "[" expr ".." expr "]"\n     | expr "[" ".." expr "]"\n     | expr "[" expr ".." "]"\nquantifier = "forall" | "exists"\n')),(0,i.kt)("p",null,"Note that ",(0,i.kt)("inlineCode",{parentName:"p"},"e1[e2]")," is part of the OCaml syntax (application of ",(0,i.kt)("inlineCode",{parentName:"p"},"e1")," to a\nsingle-element list ",(0,i.kt)("inlineCode",{parentName:"p"},"[e2]"),") but has a different meaning in Gospel, namely,\naccess to a sequence element."),(0,i.kt)("p",null,"There are two operators for logical conjunction, ",(0,i.kt)("inlineCode",{parentName:"p"},"&&")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"/\\"),", and two\noperators for logical disjunction: ",(0,i.kt)("inlineCode",{parentName:"p"},"||")," and ",(0,i.kt)("inlineCode",{parentName:"p"},"\\/"),". A difference between the two,\nif any, is tool-specific. For instance, a deductive verification tool may\ninterpret ",(0,i.kt)("inlineCode",{parentName:"p"},"A && B")," as ",(0,i.kt)("inlineCode",{parentName:"p"},"A /\\ (A -> B)")," and a runtime assertion checking tool may\ninterpret ",(0,i.kt)("inlineCode",{parentName:"p"},"A && B")," as a lazy operator (as in OCaml) and ",(0,i.kt)("inlineCode",{parentName:"p"},"A /\\ B")," as a strict\noperator."),(0,i.kt)("p",null,"A noticeable difference w.r.t. the OCaml syntax is that infix operators can be\nchained in Gospel. One can write ",(0,i.kt)("inlineCode",{parentName:"p"},"0 <= n < 100"),", for instance, and it is\ninterpreted as ",(0,i.kt)("inlineCode",{parentName:"p"},"0 <= n /\\ n < 100"),"."))}d.isMDXComponent=!0}}]);