#!/bin/bash
curl -H "Content-Type: application/json" --data '{"build": true}' -X POST https://registry.hub.docker.com/u/jkuhn/extempore_base/trigger/8d184ed1-c9bd-4212-a7d7-4e936e0c9781/

