(()=>{var k=function(n){return function(r){return function(e){return n(e)(r)}}};var u=function(n){return n.map},On=function(n){var r=u(n);return function(e){return function(a){return r(a)(e)}}};var S=function(n){return n.apply};var C=function(n){return n.pure};var Un=function(n){var r=S(n.Apply0()),e=C(n);return function(a){return function(t){return r(e(a))(t)}}};var Y=function(n){return n.bind},Lr=function(n){return k(Y(n))};var Wr=function(n){var r=Y(n.Bind1()),e=C(n.Applicative0());return function(a){return function(t){return r(a)(function(o){return r(t)(function(i){return e(o(i))})})}}};var y=function(n){return n.append};var Vt=String.fromCharCode(65535),Kt=String.fromCharCode(0),Yt=Number.POSITIVE_INFINITY,Zt=Number.NEGATIVE_INFINITY;var vn=function(n){return n.eq1},_=function(n){return n.eq};var c=function(n){return n.show};var U=function(n){return n.mempty};var Nr=function(n){return function(){return n}},Pr=function(n){return function(r){return function(){return r(n())()}}};var $r=function(n,r,e){var a=0,t;return function(o){if(a===2)return t;if(a===1)throw new ReferenceError(n+" was needed before it finished initializing (module "+r+", line "+o+")",r,o);return a=1,t=e(),a=2,t}},fr={Applicative0:function(){return Or},Bind1:function(){return lr}},lr={bind:Pr,Apply0:function(){return jr(0)}},Or={pure:Nr,Apply0:function(){return jr(0)}},Ur=$r("functorEffect","Effect",function(){return{map:Un(Or)}}),jr=$r("applyEffect","Effect",function(){return{apply:Wr(fr),Functor0:function(){return Ur(0)}}}),_n=Ur(20);var Br=function(n){return function(){return{value:n}}};var zn=function(n){return function(){return n.value}};var pr=function(n){return function(r){return function(){r.value=n}}};var Jr=Br;var da=Lr(lr),Da=u(_n),Qn=function(){function n(r){this.value0=r}return n.create=function(r){return new n(r)},n}(),qn=function(){function n(r){this.value0=r}return n.create=function(r){return new n(r)},n}(),sr=function(n){return n.tailRecM};var zr={tailRecM:function(n){return function(r){var e=function(a){if(a instanceof qn)return a.value0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 113, column 30 - line 113, column 44): "+[a.constructor.name])};return function(){var t=da(Jr)(n(r))();return function(){for(;!function(){var i=zn(t)();if(i instanceof Qn){var f=n(i.value0)();return pr(f)(t)(),!1}if(i instanceof qn)return!0;throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 104, column 22 - line 109, column 28): "+[i.constructor.name])}(););return{}}(),Da(e)(zn(t))()}}},Monad0:function(){return fr}};var kr=function(n){return function(r){return function(e){for(var a=r,t=e.length,o=t-1;o>=0;o--)a=n(e[o])(a);return a}}},Vr=function(n){return function(r){return function(e){for(var a=r,t=e.length,o=0;o<t;o++)a=n(a)(e[o]);return a}}};var cn=function(n){return n.foldr};var rn=function(n){return n.foldl};var ct=function(n){var r=cn(n);return function(e){var a=y(e.Semigroup0()),t=U(e);return function(o){return r(function(i){return function(f){return a(o(i))(f)}})(t)}}},Rn={foldr:kr,foldl:Vr,foldMap:function(n){return ct(Rn)(n)}};var Fe=function(){function n(t){return[t]}function r(t){return function(o){return[t,o]}}function e(t){return function(o){return function(i){return[t,o,i]}}}function a(t){return function(o){return t.concat(o)}}return function(t){return function(o){return function(i){return function(f){return function(p){function b(d,w){switch(w-d){case 0:return i([]);case 1:return o(n)(f(p[d]));case 2:return t(o(r)(f(p[d])))(f(p[d+1]));case 3:return t(t(o(e)(f(p[d])))(f(p[d+1])))(f(p[d+2]));default:var Q=d+Math.floor((w-d)/4)*2;return t(o(a)(b(d,Q)))(b(Q,w))}}return b(0,p.length)}}}}}}();var j=function(){function n(){}return n.value=new n,n}(),T=function(){function n(r,e){this.value0=r,this.value1=e}return n.create=function(r){return function(e){return new n(r,e)}},n}();var nr={foldr:function(n){return function(r){var e=function(){var t=function(o){return function(i){var f=o,p=!1,b;function d(w,Q){if(Q instanceof j)return p=!0,w;if(Q instanceof T){f=new T(Q.value0,w),i=Q.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): "+[w.constructor.name,Q.constructor.name])}for(;!p;)b=d(f,i);return b}};return t(j.value)}(),a=rn(nr)(k(n))(r);return function(t){return a(e(t))}}},foldl:function(n){var r=function(e){return function(a){var t=e,o=!1,i;function f(p,b){if(b instanceof j)return o=!0,p;if(b instanceof T){t=n(p)(b.value0),a=b.value1;return}throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): "+[b.constructor.name])}for(;!o;)i=f(t,a);return i}};return r},foldMap:function(n){var r=y(n.Semigroup0()),e=U(n);return function(a){return rn(nr)(function(t){var o=r(t);return function(i){return o(a(i))}})(e)}}};var He={eq1:function(n){var r=_(n);return function(e){return function(a){var t=function(o){return function(i){return function(f){var p=o,b=i,d=!1,w;function Q(Nn,Pn,tr){if(!tr)return d=!0,!1;if(Nn instanceof j&&Pn instanceof j)return d=!0,tr;if(Nn instanceof T&&Pn instanceof T){p=Nn.value1,b=Pn.value1,f=tr&&r(Pn.value0)(Nn.value0);return}return d=!0,!1}for(;!d;)w=Q(p,b,f);return w}}};return t(e)(a)(!0)}}}},no=vn(He);var Mr=function(n){return{eq:no(n)}};var to=rn(nr);var rr=function(){var n=function(r){return function(e){var a=r,t=!1,o;function i(f,p){if(p instanceof j)return t=!0,f;if(p instanceof T){a=new T(p.value0,f),e=p.value1;return}throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): "+[f.constructor.name,p.constructor.name])}for(;!t;)o=i(a,e);return o}};return n(j.value)}(),It=function(){var n=function(r){return function(e){return function(a){var t=r,o=e,i=!1,f;function p(b,d,w){if(d<1||w instanceof j)return i=!0,rr(b);if(w instanceof T){t=new T(w.value0,b),o=d-1|0,a=w.value1;return}throw new Error("Failed pattern match at Data.List (line 513, column 3 - line 513, column 35): "+[b.constructor.name,d.constructor.name,w.constructor.name])}for(;!i;)f=p(t,o,a);return f}}};return n(j.value)}();var Tt=to(function(n){return function(r){return n+1|0}})(0);var qt=function(n){return cn(n)(T.create)(j.value)};var Sn=function(n){return function(){console.log(n)}};var Ir=Math.random;var Wt=u(_n)(function(n){return n<.5})(Ir);var Rt=qt(Rn),hn=function(){function n(){}return n.value=new n,n}(),B=function(){function n(){}return n.value=new n,n}(),go={eq:function(n){return function(r){return n instanceof hn&&r instanceof hn||n instanceof B&&r instanceof B}}},Fo=_(Mr(go)),wo={show:function(n){if(n instanceof hn)return"\u30C9\u30C9";if(n instanceof B)return"\u30B9\u30B3";throw new Error("Failed pattern match at Main (line 15, column 1 - line 17, column 19): "+[n.constructor.name])}},xo=c(wo),Co=On(_n)(Wt)(function(n){return n?hn.value:B.value}),St=function(){return rr(Rt([hn.value,B.value,B.value,B.value,hn.value,B.value,B.value,B.value,hn.value,B.value,B.value,B.value]))}(),Mo=Tt(St),Eo=function(n){var r=Fo(It(Mo)(n))(St);return r?function(){return Sn("\u30E9\u30D6\u6CE8\u5165\u2661")(),new qn(void 0)}:function(){var a=Co();return Sn(xo(a))(),new Qn(new T(a,n))}},Nt=sr(zr)(Eo)(Rt([]));Nt();})();
