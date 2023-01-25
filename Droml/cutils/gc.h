#ifndef __GC_C_CONNECTOR__
#define __GC_C_CONNECTOR__

#ifdef __cplusplus
extern "C" {
#endif

    typedef char* ptr;
    typedef unsigned long long size;

    void  _removeref(ptr);
    ptr   _allocate(size);
    void  _addref(ptr);
    void  _addchild(ptr,ptr);

    void _transferchildren(ptr,ptr);

#ifdef __cplusplus
}
#endif

#endif // __GC_C_CONNECTOR__
