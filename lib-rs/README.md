# FFI bindings helpers for Rust

This crate provides utilities for bindings to FFI interfaces

## Features

* Safely create slices from C slices.
* Convenience wrapper and builder for C strings.
* Macro for creating owned data types.
* Macro for creating iterators from access by index and length.

## Example usage

```
# #[macro_use] extern crate easy_ffi_wrapper;
mod bindings {
#   use std::os::raw::c_char;
    pub enum DataHandle {}
    pub enum DataItem {}
    extern {
        pub fn data_create() -> *mut DataHandle;
        pub fn data_release(_: *mut DataHandle);
        pub fn data_clone(_: *const DataHandle) -> *mut DataHandle;
        pub fn data_numbers(_: *const DataHandle, start: *mut *const u32, size: *mut usize);
        pub fn data_name(_: *const DataHandle, start: *mut *const c_char, size: *mut usize);
        pub fn data_item_len(_: *const DataHandle) -> usize;
        pub fn data_item_get(_: *const DataHandle, pos: usize) -> *const DataItem;
    }
}
use bindings::DataHandle as Data;
use bindings::DataItem as DataItem;
easy_ffi_wrapper::ffi_box!(
    Data,
    BoxedData,
    delete(bindings::data_release),
    new(bindings::data_create),
    clone(bindings::data_clone)
);
easy_ffi_wrapper::ffi_iter!(
    ItemIter(Data) -> DataItem,
    len(bindings::data_item_len),
    get(bindings::data_item_get)
);
impl Data {
    pub fn name(&self) -> easy_ffi_wrapper::Str {
        let mut value = easy_ffi_wrapper::StrBuilder::new();
        unsafe {
            bindings::data_name(self, &mut value.ptr, &mut value.len);
            value.build()
        }
    }
    pub fn numbers(&self) -> &[u32] {
        unsafe {
            let (mut ptr, mut len) = (std::ptr::null(), 0);
            bindings::data_numbers(self, &mut ptr, &mut len);
            easy_ffi_wrapper::slice_from_raw_parts(ptr, len)
        }
    }
    pub fn items(&self) -> ItemIter
    {
        unsafe{ ItemIter::new(self) }
    }
}
```