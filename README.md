# Locks for CouchDB

This is **just experimental** locking strategy.

Currently works on single node. Any ideas how to scale?

## API

API is simply RESTful, just like Document API.

### Acquire a single lock for the scope

curl -X POST http://localhost:5984/DB/_locks?scope=Person&timeout=3000
> 201 - {"ok":true,"id":"0510eb5409dd5d3cb41fb2773f5e253e"}

Lock already exist:
> 409 - {"error":"conflict","reason":"Already exist"}

### Create document

curl -X POST http://localhost:5984/DB/\_locks/\_db/?lock=0510eb5409dd5d3cb41fb2773f5e253e -d '{"type":"Person","name":"John"}'

CouchDB replies:
> 201 - {"ok":true,"id":"5ca03037c83797a2457d13efba000c10","rev":"1-ac5e2cfb85fb3ddfc21d91334021b649"}

If lock has expired or does not exist:
> 400 - {"error":"bad_request","reason":"Lock has expired or does not exist"}

### Explicitly release a lock

curl -X DELETE http://localhost:5984/db/_locks/0510eb5409dd5d3cb41fb2773f5e253e

CouchDB replies:
> {"ok":true}

### List locks

curl -X GET http://localhost:5984/DB/_locks

CouchDB replies:
> 200 - {"total_rows":1,"rows":[{"scope":"Person","id":"0510eb5409dd5d3cb41fb2773f5e253e"}]}

## Use cases

Validate uniqueness:

  * Lock with scope "Person",
  * Ensure that there is no such person with fields intended to be unique (query your views),
  * Create document.

Convention is a key. You can use any token as scope. Use "Person" to lock all documents of type "Person".
If you want to update a Person simply don't lock unless email or login fields are dirty.

Every single lock allows only one operation, each POST, PUT, DELETE or Bulk Document API request will release the lock.
Each lock has a timeout, this means that you should not be slow, otherwise your update will be rejected.
Deadlocks are impossible.

Create an order, reduce Item#units\_in\_stock:

  * Lock with scope "Item",
  * Check each Item for availability,
  * Create an order document and update each Item via Bulk Document API.

As a bonus, conflict is impossible if all your updates are going through this API with the same scope.

## Benefits

  * Simple way to keep data in consistent state, unless you are using offline-replication,
  * Uniqueness is possible,
  * Prevent conflicts.

## How it works?

From client's point of view:

  * Try to create a lock,
  * Receive successful response,
  * Perform a number of GET or HEAD requests,
  * Perform your POST, PUT, DELETE or Bulk Document API request.

Failing scenario:

  * Try to create already existing lock,
  * Receive "409 Conflict",
  * Wait and try again.

## Installation

See INSTALL.md for installation instructions.
