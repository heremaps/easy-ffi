#![doc = include_str!("../README.md")]
#![deny(warnings, missing_debug_implementations, missing_docs)]

use std::fmt;

/// Constructs a slice from raw parts (same as [`slice::from_raw_parts`]),
/// but safely handle `{ptr: nullptr, len:0}`, which is often allowed in FFI.
///
/// # Safety
///
/// Same as [`slice::from_raw_parts`].
///
/// [`slice::from_raw_parts`]: https://doc.rust-lang.org/std/slice/fn.from_raw_parts.html
///
/// Example
/// ```
/// mod bindings {
///     pub enum Data {}
///
///     extern {
///         pub fn get_my_data_from_c(object: *const Data, start: *mut *const u32, size: *mut usize);
///     }
/// }
/// use bindings::Data as Data;
/// impl Data {
///     pub fn my_data(&self) -> &[u32] {
///         unsafe {
///             let (mut ptr, mut len) = (std::ptr::null(), 0);
///             bindings::get_my_data_from_c(self, &mut ptr, &mut len);
///             easy_ffi_wrapper::slice_from_raw_parts(ptr, len)
///         }
///     }
/// }
/// ```
pub unsafe fn slice_from_raw_parts<'a, T>(ptr: *const T, len: usize) -> &'a [T] {
    if !ptr.is_null() {
        std::slice::from_raw_parts(ptr, len)
    } else {
        std::slice::from_raw_parts(std::ptr::NonNull::dangling().as_ref(), 0)
    }
}

/// Wraps a non-owned FFI string without the overhead of CString/String
///
/// This allows the user of the API to choose if they want to treat the string
/// as a bunch of bytes ([`bytes`]), verify it as a utf8 `str` ([`as_str`]), or
/// convert it lossily into a `Cow<str>` ([`to_str_lossy`]).
///
/// # Example
///
/// ```
/// mod bindings {
/// #   use std::os::raw::c_char;
///     pub enum Data {}
///
///     extern {
///         pub fn get_my_data_from_c(object: *const Data, start: *mut *const c_char, size: *mut usize);
///     }
/// }
/// use bindings::Data as Data;
/// impl Data {
///     pub fn my_data(&self) -> easy_ffi_wrapper::Str {
///         let mut value = easy_ffi_wrapper::StrBuilder::new();
///         unsafe {
///             bindings::get_my_data_from_c(self, &mut value.ptr, &mut value.len);
///             value.build()
///         }
///     }
/// }
/// ```
#[derive(Clone, Copy)]
pub struct Str<'a> {
    data: &'a [u8],
}

impl<'a> Str<'a> {
    /// Construct a string from `ptr` + `len`.
    ///
    /// No terminating `\0` required, and invalid UTF8 possible.
    ///
    /// # Safety
    ///
    /// Cf. [`slice_from_raw_parts`].
    ///
    /// [`slice_from_raw_parts`]: fn.slice_from_raw_parts.html
    pub unsafe fn from_raw_parts(ptr: *const std::os::raw::c_char, len: usize) -> Self {
        Self {
            data: slice_from_raw_parts(ptr as *const u8, len),
        }
    }

    /// Get access to raw bytes
    pub fn bytes(&self) -> &'a [u8] {
        self.data
    }

    /// Try converting from UTF8
    pub fn to_str(&self) -> Result<&'a str, std::str::Utf8Error> {
        std::str::from_utf8(self.data)
    }

    /// Try converting from UTF8 and replace invalid characters
    pub fn to_str_lossy(&self) -> std::borrow::Cow<'a, str> {
        String::from_utf8_lossy(self.data)
    }
}

impl fmt::Debug for Str<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.to_str_lossy())
    }
}

impl<'a> std::ops::Deref for Str<'a> {
    type Target = &'a [u8];

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl AsRef<[u8]> for Str<'_> {
    fn as_ref(&self) -> &[u8] {
        self.data
    }
}

/// A convenience structure for calling FFI interfaces to get strings
///
/// # Example
///
/// ```
/// mod bindings {
/// #   use std::os::raw::c_char;
///     pub enum Data {}
///
///     extern {
///         pub fn get_my_data_from_c(object: *const Data, start: *mut *const c_char, size: *mut usize);
///     }
/// }
/// use bindings::Data as Data;
/// impl Data {
///     pub fn my_data(&self) -> easy_ffi_wrapper::Str {
///         let mut value = easy_ffi_wrapper::StrBuilder::new();
///         unsafe {
///             bindings::get_my_data_from_c(self, &mut value.ptr, &mut value.len);
///             value.build()
///         }
///     }
/// }
/// ```
#[derive(Debug)]
pub struct StrBuilder {
    /// Pointer to the first character of the string to fill in, usually as an output parameter.
    pub ptr: *const std::os::raw::c_char,
    /// Len of the string to fill in, usually as an output parameter.
    pub len: usize,
}

impl StrBuilder {
    /// Creates a new builder referencing an empty string.
    pub fn new() -> Self {
        StrBuilder {
            ptr: std::ptr::null(),
            len: 0,
        }
    }

    /// Construct a string from `ptr` + `len`.
    ///
    /// No terminating `\0` required, and invalid UTF8 possible.
    ///
    /// # Safety
    ///
    /// Same as [`slice_from_raw_parts`] with `self.ptr` and `self.len` as
    /// parameters.
    ///
    /// [`slice_from_raw_parts`]: fn.slice_from_raw_parts.html
    pub unsafe fn build<'a, 'b>(&'a self) -> Str<'b> {
        Str::from_raw_parts(self.ptr, self.len)
    }
}

impl Default for StrBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Generates an owned type `BoxedTypeName` for a FFI type `TypeName`.
///
/// The generated type `BoxedTypeName` behaves like `Box<TypeName>` with a custom destructor.
/// Box types implement: `Drop`, `Deref`, `DerefMut`, `AsRef`, `Borrow`, `Clone`, `Default`.
///
/// ## Example
///
/// ```no_run
/// # #[macro_use] extern crate easy_ffi_wrapper;
/// mod bindings {
///     pub enum TypeName {}
///
///     extern {
///         pub fn type_name_create() -> *mut TypeName;
///         pub fn type_name_release(object: *mut TypeName);
///         pub fn type_name_clone(object: *const TypeName) -> *mut TypeName;
///     }
/// }
/// use bindings::TypeName as TypeName;
/// easy_ffi_wrapper::ffi_box!(
///     TypeName,
///     BoxedTypeName,
///     debug, // optional; omit if debug output is not supported
///     delete(bindings::type_name_release),
///     new(bindings::type_name_create),  // optional default constructor
///     clone(bindings::type_name_clone)  // optional copy constructor
/// );
/// ```
///
/// ## Rationale behind
///
/// If Rust's built-in box type `Box<T>` had a facility to provide a custom destructor, we would not
/// need to generate such box types, since we could just use the built-in type.
#[macro_export]
macro_rules! ffi_box {
    (@impl, $type:ident, $boxed_type:ident, new($func:path)) => {
        impl $type {
            #[allow(clippy::new_ret_no_self)]
            pub fn new() -> $boxed_type {
                unsafe { $boxed_type { ptr: std::ptr::NonNull::new($func()).unwrap() } }
            }
        }

        impl Default for $boxed_type {
            fn default() -> $boxed_type {
                $type::new()
            }
        }
    };

    (@impl, $type:ident, $boxed_type:ident, clone($func:path)) => {
        impl Clone for $boxed_type {
            fn clone(&self) -> $boxed_type {
                self.as_ref( ).to_owned( )
            }
        }

        impl std::borrow::ToOwned for $type {
            type Owned = $boxed_type;

            fn to_owned(&self) -> Self::Owned {
                unsafe{ $boxed_type::from_raw($func(self)) }
            }
        }
    };

    ($type:ident, $boxed_type:ident, debug, delete($delete_func:path) $(, $func_type:ident($func:path))*) => {
        ffi_box!($type, $boxed_type, delete($delete_func) $(, $func_type($func))*);

        impl std::fmt::Debug for $boxed_type {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                self.ptr.fmt(f)
             }
        }
    };

    ($type:ident, $boxed_type:ident, delete($delete_func:path) $(, $func_type:ident($func:path))*) => {
        pub struct $boxed_type {
            ptr: std::ptr::NonNull<$type>,
        }

        impl $boxed_type {
            /// Take ownership of a raw pointer. Must not be null.
            ///
            /// # Safety
            ///
            /// This function is unsafe as there is no guarantee that the given pointer is valid,
            /// nor whether the lifetime inferred is a suitable lifetime for the returned object.
            pub unsafe fn from_raw( ptr: *mut $type ) -> Self {
                Self{ ptr: std::ptr::NonNull::new(ptr).unwrap() }
            }

            /// Take ownership of a raw pointer. In case of null returns None.
            ///
            /// # Safety
            ///
            /// This function is unsafe as there is no guarantee whether the lifetime inferred is a
            /// suitable lifetime for the returned object.
            pub unsafe fn from_raw_checked( ptr: *mut $type ) -> Option<Self> {
                std::ptr::NonNull::new(ptr).map(|ptr| Self{ptr})
            }

            /// Releases ownership if the raw pointer without calling the deleter.
            pub fn leak(self) -> *mut $type {
                let ptr = self.ptr.as_ptr();
                std::mem::forget( self );
                ptr
            }
        }

        impl std::borrow::Borrow<$type> for $boxed_type {
            fn borrow(&self) -> &$type {
                self.as_ref()
            }
        }

        impl Drop for $boxed_type {
            fn drop(&mut self) {
                unsafe {
                    $delete_func( self.ptr.as_ptr() );
                }
            }
        }

        impl std::ops::Deref for $boxed_type {
            type Target = $type;

            fn deref(&self) -> &Self::Target {
                unsafe{self.ptr.as_ref()}
            }
        }

        impl std::ops::DerefMut for $boxed_type {
            fn deref_mut(&mut self) -> &mut Self::Target {
                unsafe{self.ptr.as_mut()}
            }
        }

        impl AsRef<$type> for $boxed_type {
            fn as_ref(&self) -> &$type {
                unsafe{self.ptr.as_ref()}
            }
        }

        impl AsMut<$type> for $boxed_type {
            fn as_mut(&mut self) -> &mut $type {
                unsafe{self.ptr.as_mut()}
            }
        }

        $(ffi_box!(@impl, $type, $boxed_type, $func_type($func));)*
    };
}

/// Generates iterator types for FFI containers.
///
/// # Example
///
/// ```no_run
/// # #[macro_use] extern crate easy_ffi_wrapper;
/// mod bindings {
///     pub enum ContainerType {}
///     pub enum ContainerItem {}
///
///     extern {
///         pub fn container_name_len(c: *const ContainerType) -> usize;
///         pub fn container_name_get(c: *const ContainerType, pos: usize) -> *const ContainerItem;
///     }
/// }
///
/// use bindings::{ContainerType, ContainerItem};
///
/// easy_ffi_wrapper::ffi_iter!(
///     ContainerIter(ContainerType) -> ContainerItem,
///     len(bindings::container_name_len),
///     get(bindings::container_name_get)
/// );
/// ```
///
/// The generated type implements: `Iterator`, `DoubleEndedIterator`, `ExactSizeIterator`,
/// `FusedIterator`. It can be created from the container type and exposes random access `index`
/// function.
///
/// # Notes
///
/// The `std::ops::Index` trait cannot be implemented ergnomically due to the following issue:
/// The item references returned by `Index::index` cannot outlive the iterator.
/// In general `ffi_iter` is inappropriate for use with containers which already implement
/// `Iterator` (for example, types in Rust `std` and `core`).
#[macro_export]
macro_rules! ffi_iter {
    (@impl, struct, $iterator_type:ident($container:ident),) => {
        #[derive(Clone)]
        pub struct $iterator_type<'a> {
            container: &'a $container,
            pos: usize,
            len: usize,
        }
    };
    (@impl, struct, $iterator_type:ident($container:ident), mut) => {
        pub struct $iterator_type<'a> {
            container: &'a mut $container,
            pos: usize,
            len: usize,
        }
    };

    (@impl, lifetime, $iterator_type:ident,) => {
        $iterator_type<'a>
    };
    (@impl, lifetime, $iterator_type:ident, mut) => {
        $iterator_type<'_>
    };

    ($iterator_type:ident($container:ident) -> $(($mutability:ident))? $type:ident, len($len_func:path), get($get_func:path)) => {
        ffi_iter!(@impl, struct, $iterator_type($container), $($mutability)*);

        impl<'a> $iterator_type<'a> {
            /// Constructs new iterator from a given container.
            ///
            /// # Safety
            ///
            /// This function is unsafe since it uses the provided `$len_func` which is unsafe.
            /// The safety guarantees are the same as of the `$len_func` function.
            pub unsafe fn new(container: &'a $($mutability)* $container) -> Self {
                Self {
                    len: $len_func(container),
                    container,
                    pos: 0,
                }
            }

            pub fn index(&$($mutability)* self, index: usize) -> &'a $type {
                assert!(index + self.pos < self.len);
                unsafe { &$($mutability)* *$get_func(self.container, index + self.pos) }
            }

            /// Slices this iterator by a given range.
            ///
            /// # Panics
            ///
            /// Panics if the range is outside of bounds of the iterator.
            pub fn slice<R: std::ops::RangeBounds<usize>>(& $($mutability)* self, range: R) -> ffi_iter!(@impl, lifetime, $iterator_type, $($mutability)*) {
                use std::ops::Bound;
                let pos = match range.start_bound() {
                    Bound::Included(&idx) => self.pos + idx,
                    Bound::Excluded(&idx) => self.pos + idx + 1,
                    Bound::Unbounded => self.pos,
                };
                let len = match range.end_bound() {
                    Bound::Included(&idx) => self.pos + idx + 1,
                    Bound::Excluded(&idx) => self.pos + idx,
                    Bound::Unbounded => self.len,
                };
                $iterator_type {
                    container: self.container,
                    pos,
                    len,
                }
            }
        }

        impl<'a> Iterator for $iterator_type<'a> {
            type Item = &'a $($mutability)* $type;
            fn next(&mut self) -> Option<Self::Item> {
                if self.pos < self.len {
                    let result = unsafe { &$($mutability)* *$get_func(self.container, self.pos) };
                    self.pos += 1;
                    Some(result)
                } else {
                    None
                }
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                (self.len - self.pos, Some(self.len - self.pos))
            }
        }

        impl<'a> DoubleEndedIterator for $iterator_type<'a> {
            fn next_back(&mut self) -> Option<Self::Item> {
                if self.pos < self.len {
                    let result = unsafe { &$($mutability)* *$get_func(self.container, self.len - 1) };
                    self.len -= 1;
                    Some(result)
                } else {
                    None
                }
            }
        }

        impl<'a> std::iter::FusedIterator for $iterator_type<'a> {}

        impl<'a> ExactSizeIterator for $iterator_type<'a> {}
    };
}

/// Generates iterator types for FFI containers.
///
/// # Example
///
/// ```rust,no_run
/// # #[macro_use] extern crate easy_ffi_wrapper;
/// mod bindings {
///     pub enum ContainerType {}
///     pub enum ContainerItem {}
///
///     extern {
///         pub fn container_name_len(c: *const ContainerType) -> usize;
///         pub fn container_name_get(c: *const ContainerType, pos: usize) -> *const ContainerItem;
///         pub fn container_name_get_mut(c: *mut ContainerType, pos: usize) -> *mut ContainerItem;
///         pub fn container_name_grow(c: *mut ContainerType) -> *mut ContainerItem;
///         pub fn container_name_clear(c: *mut ContainerType);
///     }
/// }
///
/// use bindings::{ContainerType, ContainerItem};
///
/// easy_ffi_wrapper::ffi_vec!(
///     ContainerType(ContainerIter, ContainerIterMut) -> ContainerItem,
///     len(bindings::container_name_len),
///     get(bindings::container_name_get),
///     get_mut(bindings::container_name_get_mut),
///     grow(bindings::container_name_grow),
///     clear(bindings::container_name_clear)
/// );
/// ```
#[macro_export]
macro_rules! ffi_vec {
    ($vec_type:ident($iter_type:ident, $iter_mut_type:ident) -> $type:ident,
        len($len_func:path),
        get($get_func:path),
        get_mut($get_mut_func:path),
        grow($grow_func:path),
        clear($clear_func:path)) => {
        $crate::ffi_iter!(
          $iter_type($vec_type) -> $type,
          len($len_func),
          get($get_func)
        );
        $crate::ffi_iter!(
          $iter_mut_type($vec_type) -> (mut) $type,
          len($len_func),
          get($get_mut_func)
        );
        impl $vec_type {
            /// Returns an iterator over the content
            pub fn iter(&self) -> $iter_type {
                unsafe {$iter_type::new(self)}
            }

            /// Returns a mutable iterator over the content
            pub fn iter_mut(&mut self) -> $iter_mut_type {
                unsafe {$iter_mut_type::new(self)}
            }

            pub fn grow(&mut self) -> &mut $type {
                unsafe {&mut *$grow_func(self)}
            }

            pub fn clear(&mut self) {
                unsafe {$clear_func(self)};
            }

            pub fn len(&self) -> usize{
                unsafe {$len_func(self)}
            }

            pub fn get(&self, index: usize) -> Option<&$type> {
                if index >= self.len() {
                    None
                } else {
                    unsafe {Some(&*$get_func(self, index))}
                }
            }

            pub fn first(&self) -> Option<&$type> {
                self.get(0)
            }

            pub fn first_mut(&mut self) -> Option<&mut $type> {
                self.get_mut(0)
            }

            pub fn last(&self) -> Option<&$type> {
                self.get(self.len().wrapping_sub(1))
            }

            pub fn last_mut(&mut self) -> Option<&mut $type> {
                self.get_mut(self.len().wrapping_sub(1))
            }

            pub fn get_mut(&mut self, index: usize) -> Option<&mut $type> {
                if index >= self.len() {
                    None
                } else {
                    unsafe {Some(&mut *$get_mut_func(self, index))}
                }
            }
        }

        impl std::ops::Index<usize> for $vec_type {
            type Output = $type;

            fn index(&self, index: usize) -> &$type {
                self.get(index).expect("Out of bounds")
            }
        }

        impl std::ops::IndexMut<usize> for $vec_type {
            fn index_mut(&mut self, index: usize) -> &mut $type {
                self.get_mut(index).expect("Out of bounds")
            }
        }
    };
}

#[cfg(test)]
mod tests {
    #![allow(dead_code)]
    use super::*;

    #[derive(Debug)]
    pub struct A {}

    unsafe fn create_a() -> *mut A {
        Box::leak(Box::new(A {}))
    }

    unsafe fn delete_a(ptr: *mut A) {
        std::mem::drop(Box::from_raw(ptr));
    }

    ffi_box!(A, BoxedA, debug, delete(delete_a), new(create_a));

    #[test]
    fn boxed_new() {
        {
            let _a = A::new();
        }
    }

    #[test]
    fn debug_a() {
        format!("{:?}", A::new());
    }

    pub struct B {}

    unsafe fn create_b() -> *mut B {
        Box::leak(Box::new(B {}))
    }

    unsafe fn delete_b(ptr: *mut B) {
        std::mem::drop(Box::from_raw(ptr));
    }

    ffi_box!(B, BoxedB, delete(delete_b));

    #[test]
    fn boxed_from_raw() {
        {
            let b = unsafe { BoxedB::from_raw(create_b()) };
            unsafe {
                delete_b(b.leak());
            }
        }
    }

    pub struct C {}

    unsafe fn create_c() -> *mut C {
        Box::leak(Box::new(C {}))
    }

    unsafe fn delete_c(ptr: *mut C) {
        std::mem::drop(Box::from_raw(ptr));
    }

    unsafe fn clone_c(_ptr: *const C) -> *mut C {
        Box::leak(Box::new(C {}))
    }

    ffi_box!(C, BoxedC, delete(delete_c), new(create_c), clone(clone_c));

    #[test]
    fn boxed_clone() {
        {
            let c = C::new();
            let _d = c.clone();
            let _e = (&c as &C).to_owned();
        }
    }

    pub struct MyVec(Vec<usize>);

    unsafe fn vec_len(ptr: *const MyVec) -> usize {
        (*ptr).0.len()
    }

    unsafe fn vec_get(ptr: *const MyVec, index: usize) -> *const usize {
        &(*ptr).0[index]
    }

    ffi_iter!(MyVecIter(MyVec) -> usize, len(vec_len), get(vec_get));

    #[test]
    fn iter() {
        let data = MyVec(vec![0, 1, 2, 3, 4, 5, 6, 7]);
        let data_rev = MyVec(data.0.iter().cloned().rev().collect());
        let mut iter = unsafe { MyVecIter::new(&data) };
        let collected: Vec<_> = iter.clone().cloned().collect();
        assert_eq!(collected, data.0);
        let collected_rev: Vec<_> = iter.clone().rev().cloned().collect();
        assert_eq!(collected_rev, data_rev.0);

        assert_eq!(data.0.len(), iter.len());
        for x in 0..=7 {
            assert_eq!(x, *iter.index(x));
        }
        iter.next();
        assert_eq!(data.0.len() - 1, iter.len());
        for x in 0..=6 {
            assert_eq!(x + 1, *iter.index(x));
        }

        let x;
        {
            let iter_scoped = unsafe { MyVecIter::new(&data) };
            x = iter_scoped.index(2);
        }
        assert_eq!(x, &data.0[2]);
    }

    #[test]
    fn iter_slice() {
        let data = MyVec(vec![0, 1, 2, 3, 4, 5, 6, 7]);
        let iter = unsafe { MyVecIter::new(&data) };

        let mut sliced_once = iter.slice(1..7);
        let mut sliced_twice = sliced_once.slice(1..5);
        assert_eq!(sliced_once.index(0), &1);
        assert_eq!(sliced_once.index(5), &6);
        assert_eq!(sliced_once.next(), Some(&1));
        assert_eq!(sliced_once.next(), Some(&2));
        assert_eq!(sliced_once.next(), Some(&3));
        assert_eq!(sliced_once.next(), Some(&4));
        assert_eq!(sliced_once.next(), Some(&5));
        assert_eq!(sliced_once.next(), Some(&6));
        assert_eq!(sliced_once.next(), None);

        assert_eq!(sliced_twice.index(0), &2);
        assert_eq!(sliced_twice.index(3), &5);
        assert_eq!(sliced_twice.next(), Some(&2));
        assert_eq!(sliced_twice.next(), Some(&3));
        assert_eq!(sliced_twice.next(), Some(&4));
        assert_eq!(sliced_twice.next(), Some(&5));
        assert_eq!(sliced_twice.next(), None);

        let sliced_unbounded = iter.slice(..);
        assert_eq!(sliced_unbounded.index(0), &0);
        assert_eq!(sliced_unbounded.index(7), &7);
        assert_eq!(sliced_unbounded.copied().collect::<Vec<_>>(), data.0);

        let sliced_inclusive = iter.slice(0..=7);
        assert_eq!(sliced_inclusive.index(0), &0);
        assert_eq!(sliced_inclusive.index(7), &7);
        assert_eq!(sliced_inclusive.copied().collect::<Vec<_>>(), data.0);
    }

    #[test]
    fn empty_str() {
        let empty1 = unsafe { Str::from_raw_parts(std::ptr::null(), 10) };
        assert_eq!(empty1.bytes(), b"");
        let empty2 = Some(unsafe { Str::from_raw_parts(std::ptr::null(), 10) });
        assert!(empty2.is_some()); // This would fail for unchecked slices and just eval to `None`
    }

    #[test]
    fn str_utf8() {
        let str_ascii =
            unsafe { Str::from_raw_parts(b"abcde" as *const _ as *const std::os::raw::c_char, 5) };
        assert_eq!(str_ascii.to_str(), Ok("abcde"));
        let str_utf8 = unsafe {
            Str::from_raw_parts(
                b"abc\xce\xb1de" as *const _ as *const std::os::raw::c_char,
                7,
            )
        };
        assert_eq!(str_utf8.to_str(), Ok("abcαde"));
        assert_eq!(
            str_utf8.to_str_lossy(),
            std::borrow::Cow::Borrowed("abcαde")
        );
        let str_invalid_utf8 = unsafe {
            Str::from_raw_parts(
                b"abc\x00\xb1de" as *const _ as *const std::os::raw::c_char,
                7,
            )
        };
        assert!(str_invalid_utf8.to_str().is_err());
        match str_invalid_utf8.to_str_lossy() {
            std::borrow::Cow::Owned(x) => {
                assert_eq!(x, String::from("abc\0�de"));
            }
            _ => panic!("UTF decoding should have failed"),
        }
    }

    #[test]
    fn str_builder() {
        struct A {}

        impl A {
            fn to_str_with_bound_lifetime(&self) -> &str {
                let mut result = StrBuilder::new();
                result.ptr = b"bla".as_ptr() as *const _;
                result.len = 3;

                unsafe { result.build().to_str().unwrap() }
            }
        }
    }

    unsafe fn vec_get_mut(c: *mut MyVec, pos: usize) -> *mut usize {
        &mut (*c).0[pos]
    }
    unsafe fn vec_grow(c: *mut tests::MyVec) -> *mut usize {
        (*c).0.push(Default::default());
        &mut *(*c).0.last_mut().unwrap()
    }
    unsafe fn vec_clear(c: *mut MyVec) {
        (*c).0.clear();
    }

    ffi_vec!(
        MyVec(MyVecIterConst, MyVecIterMut) -> usize,
        len(vec_len),
        get(vec_get),
        get_mut(vec_get_mut),
        grow(vec_grow),
        clear(vec_clear)
    );

    #[test]
    fn vec() {
        let mut data = MyVec(vec![0, 1, 2, 3, 4, 5, 6, 7]);
        assert_eq!(data.len(), 8);
        data[0] = 10;
        assert_eq!(data[0], 10);
        assert_eq!(data.first(), Some(&10));
        *data.last_mut().unwrap() = 99;
        assert_eq!(data[7], 99);
        assert_eq!(data.last(), Some(&99));
        data.clear();
        assert_eq!(data.len(), 0);
        *data.grow() = 100;
        assert_eq!(data.len(), 1);
        assert_eq!(data[0], 100);
    }
}
