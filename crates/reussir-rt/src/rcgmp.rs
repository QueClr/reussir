//! An owned handle of a `Nat` or an `Int`, for Rust code that holds the
//! integers as its own values, such as the textures of the polymorphic FFI.
//!
//! An [`Mpz`] owns one reference of an rcgmp integer
//! (`RCGMP/Runtime/Mpz.h`): cloning retains it and dropping releases it,
//! through the rcgmp runtime's C entry points, and it has the layout of the
//! handle itself (`#[repr(transparent)]`). Counting is nonatomic: an `Arc`
//! coloring never crosses the FFI boundary. Comparisons read the values
//! (`rcgmp_mpz_cmp`), so ordered containers keyed by integers order them by
//! value.
//!
//! Every method that calls the rcgmp runtime is `#[inline]`, so it is compiled
//! only into the code that uses it: this crate itself references no rcgmp
//! symbol, and a program without integers links without the rcgmp runtime.

use core::cmp::Ordering;
use core::ffi::c_void;

unsafe extern "C" {
    fn rcgmp_mpz_retain(value: *mut c_void, atomic: u32) -> *mut c_void;
    fn rcgmp_mpz_release(value: *mut c_void, atomic: u32);
    fn rcgmp_mpz_cmp(lhs: *mut c_void, rhs: *mut c_void) -> i32;
}

/// One owned, nonatomically counted reference of a `Nat` or an `Int`.
#[repr(transparent)]
pub struct Mpz(*mut c_void);

impl Mpz {
    /// Takes over the reference `handle` owns.
    ///
    /// # Safety
    ///
    /// `handle` must be a canonical rcgmp handle owning one nonatomic
    /// reference, which the result owns from now on.
    #[inline]
    pub unsafe fn from_raw(handle: *mut c_void) -> Self {
        Self(handle)
    }

    /// Gives up the reference as the raw handle, without releasing it.
    #[inline]
    pub fn into_raw(self) -> *mut c_void {
        let handle = self.0;
        core::mem::forget(self);
        handle
    }

    /// The raw handle, borrowed: the reference stays with `self`.
    #[inline]
    pub fn as_ptr(&self) -> *mut c_void {
        self.0
    }
}

impl Clone for Mpz {
    #[inline]
    fn clone(&self) -> Self {
        Self(unsafe { rcgmp_mpz_retain(self.0, 0) })
    }
}

impl Drop for Mpz {
    #[inline]
    fn drop(&mut self) {
        unsafe { rcgmp_mpz_release(self.0, 0) }
    }
}

impl PartialEq for Mpz {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Mpz {}

impl PartialOrd for Mpz {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Mpz {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        unsafe { rcgmp_mpz_cmp(self.0, other.0) }.cmp(&0)
    }
}

#[cfg(test)]
mod tests {
    use super::Mpz;
    use core::ffi::c_void;
    use core::mem::ManuallyDrop;

    /// The handle has the layout of a pointer, and `from_raw` and `into_raw`
    /// hand its reference over unchanged. No count is touched: the crate's
    /// tests do not link the rcgmp runtime.
    #[test]
    fn handle_is_one_pointer_and_round_trips() {
        assert_eq!(size_of::<Mpz>(), size_of::<*mut c_void>());
        assert_eq!(align_of::<Mpz>(), align_of::<*mut c_void>());
        // The tagged bits of the immediate 1 (`4 * x + 1`).
        let raw = core::ptr::without_provenance_mut::<c_void>(5);
        let handle = ManuallyDrop::new(unsafe { Mpz::from_raw(raw) });
        assert_eq!(handle.as_ptr(), raw);
        assert_eq!(ManuallyDrop::into_inner(handle).into_raw(), raw);
    }
}
