#ifndef __GC_C_CONNECTOR__
#define __GC_C_CONNECTOR__

#ifdef __cplusplus
extern "C" {
#endif

    typedef char* ptr;
    typedef unsigned long long size;

    void  _removeref(ptr);
    ptr   _allocate(size);
    void  _addchild(ptr,ptr);

#ifdef __cplusplus
}
#endif

#endif // __GC_C_CONNECTOR