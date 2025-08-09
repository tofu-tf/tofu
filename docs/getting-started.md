---
title: Getting started
sidebar:
    order: 1
---

The library consists of a few modules, which can be installed all at once:  
:::tip[Tofu]{icon="download"}
```
"tf.tofu" %% "tofu" % "@VERSION@"
```  
:::
or as standalone dependencies:

### [Core](/docs/core/errors/)
```
"tf.tofu" %% "tofu-core-*" % "@VERSION@"

```
(replace suffix `*` with `ce2` or `ce3` depends on which cats-effect version you use)

### [Concurrent](/docs/concurrent/agent/)
```
"tf.tofu" %% "tofu-concurrent" % "@VERSION@"
``` 

### [Config](/docs/utilities/config/)
```
"tf.tofu" %% "tofu-config" % "@VERSION@"
``` 

### [Env](/docs/contextual/env/)
```
"tf.tofu" %% "tofu-env" % "@VERSION@"
``` 

### [Logging](/docs/logging/quickstart/)
```
"tf.tofu" %% "tofu-logging" % "@VERSION@"
``` 


### [Memo](/docs/utilities/memo/)
```
"tf.tofu" %% "tofu-memo" % "@VERSION@"
``` 

### [Optics](/docs/optics/optics/)
```
"tf.tofu" %% "tofu-optics-core" % "@VERSION@"
"tf.tofu" %% "tofu-optics-macro" % "@VERSION@"
"tf.tofu" %% "tofu-optics-interop" % "@VERSION@"
``` 

