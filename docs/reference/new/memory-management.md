**IGNORE Not ready for prime time**

---
title: Memory Management
---

The two languages hosted by the Extempore compiler, xtlang and Scheme, have
different approaches to dealing with memory allocation and management. While
Scheme manages memory for you, in xtlang you have to do it yourself. The tradeoff is that while this takes more work, it means that we can write highly performant code that can process real-time audio and video.

In xtlang whenever you need memory you ask the compiler for it, and when you're finished with it you tell the compiler to free the memory. This results in highly efficient code, but the downside is that if you forget to tell the compiler to free memory you can end up running out of memory pretty quickly. Fortunately xtlang has some good tools that can help you manage this.

## Types of Memory

In the chapter on functions we saw that there are two types of memory accessible to your program - the stack and the heap.

### The Stack

The stack is a special region of your computer's RAM that stores temporary variables. Each time you call a function and stack variables created in that function are pushed onto the top of the stack. When the function returns, those variables are deleted (freeing the memory that they occupied).

There are two advantages to using stack memory. Firstly you don't need to worry about freeing these variables as the OS takes care of this for you. The other advantage is that read/write access to stack variables is typically very fast.

Unfortunately stack memory is usually quite limited and if you run out of stack space your program will crash. So stack memory is not suitable if you need a lot of memory (e.g. an audio buffer). Stack data is also unsuitable if you have data that needs to persist after a function has returned. In these situations you need to store data on the heap.

### The Heap

All other data is stored on the heap. Any data stored on the heap exists until you explicitly tell the OS to delete it. If you neglect to free memory when you no longer need it your program will slowly use more and more memory (this is called 'leaking memory') bringing your program to a halt (slower programming languages typically manage memory for you with a garbage collector, but this doesn't work well for real time domains such as video and sound).

When working in xtlang you will find that you very rarely store memory directly in the heap. Instead you will use 'zones' of memory. XTLang's zones are regions of heap memory that allow you to both manage memory more efficiently and manage memory automatically (e.g. zones can give you some of the advantages of stack memory)

## Stack Memory

You should stack memory when you have data where:

+ The data isn't needed outside the function.
+ The data doesn't need much storage

The advantages of using the stack for this kind of data are:

+ Access and allocation to this data are very fast.
+ You don't have to worry about deleting it when the function returns.

For core types, such as integers and floats, stack allocation happens by default. Every time you've written code that looks like:

```xtlang
(bind-func simple_stack_alloc
  (lambda ()
    (let ((a 2) ;; stack allocated
          (b 3.5)) ;; stack allocated
      (* a b))))
```

you have put data on the stack. However in the following example `point` is allocated on the heap:

```xtlang
(bind-type Point <double,double>)

(bind-func test-point
  (lambda ()
    (let ((point:Point* (Point 3.0 3.5)))
       (println "x: " (tref point 0) "y: " (tref point 1)))))
```

In general if a constructor returns a _pointer_ to an object then that object is allocated on the heap. If you want to allocate a custom datatype on the stack you need to add '_val' to the end of the constructor function. In the example below we use `Point_val` to create a point datatype on the stack:

```xtlang
(bind-func test-point
  (lambda ()
    (let ((point:Point (Point_val 3.0 3.5))) ;; Allocating on the stack
       (println "x: " (tref point 0) "y: " (tref point 1)))))
```

`point` is deleted by the OS when `test-point` returns without us having to worry about it.

We can also reserve space on the stack for multiple points using `salloc`:

```xtlang
(bind-func test-point
  (lambda ()
    (let ((points:Point* (salloc 3))) ;; Allocating space on the stack
      (begin
        (doloop (i 3)
          (let ((point (tref points i)))
            (tref point 0 (*i 10))
            (tref point 1 (*i 10))))
        (doloop (i 3)
          (let ((point (tref points i)))
            (println "x: " (tref point 0) "y: " (tref point 1)))))
```

`salloc` allocated a memory block large enough to store 3 `Point` datatypes and returned a _pointer_ to that memory block [fn:: For C programmers note that Extempore allocates memory using the size of the type, rather than in bytes like C and C++]. It is then up to us to create the objects that go in that data block.

Once the program has left the `let` form the OS frees the memory used by `points` and that data is no longer available.

We can also use `salloc` to allocate memory blocks that can store primitive data types such as floats, or integers:

```xtlang
(bind-func buffer_stack_alloc
  (lambda ()
    (let ((buf:float* (salloc 16)))  ;; buffer of 16 float objects
      (let ((i 0))  ;; set each value in buf to 1.0
        (dotimes (i size)
          (pset! buf i 1.0)))))) ;; set position i to 1.0
```

Note that salloc needs to know the type of the data in order to allocate enough space. In the example above is sees that `buf` has type `float*` and so allocates enough memory for 16 floating point numbers.

Note that xtlang will not stop you from returning `buf`. That's because `buf` is __not__ the memory that you allocated for your buffer. Instead it is a _pointer_ to the location where the buffer is stored. When xtlang frees that memory, that location in memory still exists, but it is no longer reserved for your audio buffer and could be absolutely anything. We will discuss approaches for returning stack allocated objects from a function later in this chapter.

## Zone Memory

A zone is a special region of the heap that can be freed as a single block. A memory zone will also grow as necessary in order to store the data that you put into it. As we'll see in this section this gives us the power of heap allocation with the convenience of stack memory.

You allocate memory to a zone in XTLang using `alloc` (this is actually an alias for `zalloc`):

```xtlang
(bind-type Point <double,double>)

(bind-func test-zalloc
  (lambda ()
    (let ((a:double* (alloc 4)))
      a)))
```

Here we create a block of 4 doubles and store a pointer to it in `a`. But where does `a` live? Each thread of execution in xtlang [fn:: A thread is what is calling your functions] has a stack of memory zones, with a default memory zone at the bottom. If you not explicitly told xtlang which memory zone to use, then xtlang will use the default zone. In the next example we'll use the function `peek-zone` to look at this:

```xtlang
(bind-func test-zalloc2
  (lambda ()
    (println (peek_zone))
    (test-zalloc)  ;; test-zalloc is now allocating memory in our new zone
    (println (peek_zone))
    void))
```

If you call `test-zalloc2` then you will see that the zone is getting smaller by 64 bytes each time that you call it (with each double requiring 16 bytes). We have no way to reclaim this memory so we are effectively leaking memory. If we want to be able to reclaim this memory, then we need to call `test-zalloc` from within a new zone and then destroy the zone at the end:

```xtlang
(bind-func test-zalloc2
  (lambda ()
    (println (peek_zone))
    (push_new_zone 1024)  ;; create a new zone of size 1024 bytes and use it.
    (println (peek_zone))
    (test-zalloc)  ;; test-zalloc is now allocating memory in our new zone
    (println (peek_zone))
    (destroy_zone (pop_zone)) ;; return to the default zone and destroy the zone we just created.
    (println (peek_zone))
    void))
```

This time we create a zone (of size 1024 bytes) and make this our default zone with `push_new_zone`. We then call `test-zalloc` from within the zone, before calling `pop_zone` (which returns us to our original zone). Note that we have to explicitly _destroy_ the zone with `destroy_zone` to free the memory that was allocated.

If you run this new function you'll see that we've patched the leak.

For convenience we can rewrite the above function into a much simpler form:

```xtlang
(bind-func test-zalloc2
  (lambda ()
    (println (peek_zone)) ;; in the
    (beginz 32000;; enter a new zone
     (println (peek_zone))
     (test-zalloc)  ;; test-zalloc is now allocating memory in our new zone
     (println (peek_zone))
     void)
    (println (peek_zone)) ;; returned to the default zone
    void))
```

Now we don't have to worry about managing our zone, it's taken care of for us by the `beginz` code block. As soon as we leave the block the zone is destroyed. We can even return values from our `beginz` block to the zone above:

```xtlang
(bind-func test1
  (lambda ()
    (let ((a:double* (alloc 1000000))
          (b:double*: (alloc 4)))
      b)))

(bind-func test2
  (lambda ()
    (println (peek_zone))
    (let ((x:double*
           (beginz
            (test1)  ;; return the result to the enclosing zone
            )))
      x)
    (println (peek_zone))
    void))
```

In the code above `test1` is called from the zone created by `beginz` in `test2`. All memory allocation occurs in that zone. Then test1 returns `b`, a pointer to a memory block containing 4 float objects. The values contained in those floats are then _copied_ to the enclosing zone (which in this case is the default zone) and a pointer to the _new_ memory block is assigned to x. That is if you return a pointer, any data that is accessible from that pointer will be copied to the enclosing block (this is known as deep copying).

In other words, you can safely return values from a `beginz` by placing them on the last line in the block. This can be very useful as it allows you to do all kinds of processing, return the result and clean up all the interim data. As you'll see later this is a common pattern that you will use a lot.

Another form that you will use a lot with zones is `letz`

```xtlang
(bind-func letz_example
  (lambda ()
    (letz ((a:double* (alloc 5))  ;; letz creates a new zone and creates these variables
           (b:double* (alloc 6000)))
      a))) ;; 'return' a to the zone above

(bind-func call_letz
  (lambda ()
    (letz_example)))
```

The `letz` block above could equally be rewritten:

```xtlang
(beginz
  (let ((a:double* (alloc 5))  ;; letz creates a new zone and creates these variables
        (b:double* (alloc 6000)))
    a))
```

but this is just a slightly cleaner way of writing the same thing.

### Creating Aggregate Types in the Zone

So far we've just been creating blocks of primtive types in a zone, but we can also create more complex user defined types there as well:

```xtlang
(bind-type Point <double,double>)

(bind-func test-point
  (lambda ()
    (let ((point:Point* (Point 3.0 3.5))) ;; allocated in the current zone.
      (println "x: " (tref point 0) "y: " (tref point 1)))))
```

When creating a single object there's no need to use alloc, we can just use the default constructor and xtlang takes care of it for us.

### Advanced Zone Manipulation

Most of the time the zone functions we've already discussed will be enough. However there are some additional functions that you may occasionally need:

+ `Zone:[mzone*,i64]` - create a zone with a size of x bytes and returns a pointer to it.
+ `destroy_zone:[void,mzone*]` - frees the memory in the zone and then destroys the zone
+ `reset_zone:[mzone*,mzone*]` - frees the memory in the zone resetting it to its initial state.
+ `push_zone:[void,mzone*]` - make this zone the currently active zone.
+ `push_new_zone:[mzone*,i64]*` - create a new zone of size x bytes, make it the active zone and return a pointer to it
+ `pop_zone:[mzone*]` - uses the previously active zone.
+ `peek_zone:[mzone*]` - returns a pointer to the currently active zone

An important thing to note about zones is that we don't just make a zone active, but we 'push' it. In other words the current thread of execution maintains a stack of zones, with the default zone at the bottom of the stack. As we'll see later in this chapter this can be quite powerful.
#### Copying Between Zones

You can copy a piece of data from one zone to another using `zcopy`. `zcopy` takes 3 parameters:

+ `value` - the value that you are copying from the source zone.
+ `fromZone` - the source zone.
+ `toZone` - the desination zone.

The function returns a pointer to the value in the destination zone. So for example:

```xtlang
(let ((destData (zcopy srcData sourceZone destZone)))
  .... ;; do something exciting with destData)
```

#### Destructor Functions

Sometimes you don't just memory allocated with a zone, but also other resources that need to be cleaned up. For example you might have file handles, or network sockets, open. Xt_lang provides a cleanup hook which is run just before the zone is destroyed `zone_cleanup`

```xtlang
(bind-func cleanup
  (lambda ()
    (println "Entering 'cleanup' function")
    (let ((tmp2:i8* (alloc 8)))
      (zone_cleanup (println "Clean up before leaving zone!"))
      (println "do something")
      (println "leaving 'cleanup' function")
      void)))

(bind-func cleanup-call
  (lambda ()
    (beginz
     (println "In Zone ...")
     (zone_cleanup (println "Clean up some additional stuff before leaving!"))
     (cleanup)
     void)
    (println "Out of zone ...")
    void))
```

Whenever you pass `zone_cleanup` a block of code, xtlang registers this block with the currently active zone. When this zone is deleted your block of code will be run. As you can see in the example above you can register as many blocks of code as you like and each of them will be run (note that no guarantees are made as to the order in which they will be run).

#### Closures and Zones

When you create a top level function with `bind-func` xtlang makes use of a special type of zone:

```xtlang
(bind-func buf_func
  (let ((buf:double* (alloc 10)))
    (doloop (i 10) (pset! buf i 0.0)) ;; 0 set the buffer
    (lambda (pos)
      (pref buf pos))))
```

In this example a zone is created when `buf_func` is compiled and buf is allocated in that zone. This zone persists _unless_ you recompile `buf_func` at some future point in time. At which point the original zone will be destroyed, the memory freed and then a new zone will be created for the new closure.

### Heap Allocation

You should almost never need to use the heap directly, and usually when you're tempted to do so it's a sign that you need to rethink the design of your code. However sometimes it is necessary and so in this section we'll show you the tools available to you.

To explicitly create an object in the heap add '_h' on the end of the constructor like so:

```xtlang
(bind-func test-point
  (lambda ()
    (let ((point:Point* (Point_h 3.0 3.5)))
       (println "x: " (tref point 0) "y: " (tref point 1)))))
```

If you want to create a memory block on the heap you can do this using `halloc`:

```xtlang
(bind-type Point <double,double>)

(bind-func test-point
  (lambda ()
    (let ((points:Point* (halloc 3))) ;; Allocating space on the heap
      (begin
        (doloop (i 3)
          (let ((point (pref points i)))
            (tref point 0 (* i 10))
            (tref point 1 (* i 10))))
        (doloop (i 3)
          (let ((point (pref points i)))
            (println "x: " (tref point 0) "y: " (tref point 1))))))))
```

Note that other than the call to `halloc`, this code is identical to the code above where we used `alloc`. Dereferencing a pointer is identical for heap, zone and stack allocated objects.

However there's a big problem with the two functions that we've written above - they leak memory. Every call to the function will use more memory until our program runs out of heap space (which could bring our computer to a halt). We need to clean up after ourselves. We do this with `free`.

```xtlang
(bind-func test-point
  (lambda ()
    (let ((points:Point* (halloc 3))) ;; Allocating space on the heap
      (begin
        (doloop (i 3)
          (let ((point (pref points i)))
            (tref point 0 (* i 10))
            (tref point 1 (* i 10))))
        (doloop (i 3)
          (let ((point (pref points i)))
            (println "x: " (tref point 0) "y: " (tref point 1))))
        (free points))))) ;; free points as we no longer need this data
```

### Conclusion

In this chapter we've looked at the three options available to use for allocating memory in xtlang: stack allocation, zone allocation and heap allocation. In the next chapter we'll discuss the best ways to manage memory in xtlang.
