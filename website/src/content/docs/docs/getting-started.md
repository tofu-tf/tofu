---
title: Getting started
sidebar:
    order: 1
---

The library consists of a few modules, which can be installed all at once:  
:::tip[Tofu]{icon="download"}
```
"tf.tofu" %% "tofu" % "@VERSION"
```  
:::
or as standalone dependencies:

### [Core](/tofu/docs/core/errors/)
```
"tf.tofu" %% "tofu-core-*" % "@VERSION"

```
(replace suffix `*` with `ce2` or `ce3` depends on which cats-effect version you use)

### [Concurrent](/tofu/docs/concurrent/agent/)
```
"tf.tofu" %% "tofu-concurrent" % "@VERSION"
``` 

### [Config](/tofu/docs/utilities/config/)
```
"tf.tofu" %% "tofu-config" % "@VERSION"
``` 

### [Env](/tofu/docs/contextual/env/)
```
"tf.tofu" %% "tofu-env" % "@VERSION"
``` 

### [Logging](/tofu/docs/logging/quickstart/)
```
"tf.tofu" %% "tofu-logging" % "@VERSION"
``` 


### [Memo](/tofu/docs/utilities/memo/)
```
"tf.tofu" %% "tofu-memo" % "@VERSION"
``` 

### [Optics](/tofu/docs/optics/optics/)
```
"tf.tofu" %% "tofu-optics-core" % "@VERSION"
"tf.tofu" %% "tofu-optics-macro" % "@VERSION"
"tf.tofu" %% "tofu-optics-interop" % "@VERSION"
``` 

