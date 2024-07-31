# streampatch
streampatch is a Haskell library for defining patches and applying them over
streams.

This is the minimal base for bytepatch, which is an application that supports
patching streams using various patch types and patch checks. That entails lots
of arbitrary dependencies, while much of the basic design is standalone. So that
standalone design is placed here.
