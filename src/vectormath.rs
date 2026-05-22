
use core::f32;
use core::ops::{
    Add,
    AddAssign,
    Mul,
    MulAssign,
    Neg,
    Sub,
    SubAssign,
};
use core::fmt::Display;

pub trait Lerp<Rhs = Self> where Self: Sized {
    type Output;
    fn lerp(self, rhs: Rhs, alpha: f32) -> Self::Output;
}

impl<T> Lerp<T> for T
where
    T: Mul<f32, Output = T>,
    <T as Mul<f32>>::Output: Add<T>,
{
    type Output = <T as Add<T>>::Output;

    fn lerp(self, rhs: T, alpha: f32) -> Self::Output {
        self * (1.0 - alpha) + rhs * alpha
    }
}

/** \ingroup Structs
 * A four element, floating point quaternion. @since 3.1.2
 */
#[repr(C)]
#[derive(Clone, Copy)]
pub struct Quaternion {
    /** The x coefficient of the vector portion of the quaternion. @since 3.1.2 */
    pub i: f32,
    /** The y coefficient of the vector portion of the quaternion. @since 3.1.2 */
    pub j: f32,
    /** The z coefficient of the vector portion of the quaternion. @since 3.1.2 */
    pub k: f32,
    /** The scalar portion of the quaternion. @since 3.1.2 */
    pub r: f32,
}
impl Default for Quaternion {
    fn default() -> Self {
        Self { i: 0.0, j: 0.0, k: 0.0, r: 1.0 }
    }
}

impl core::fmt::Debug for Quaternion {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "Quaternion{{x:{:09.7}, y:{:09.7}, z:{:09.7}, w:{:09.7}}}", self.i, self.j, self.k, self.r)
    }
}
impl core::fmt::Display for Quaternion {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "[{:09.3}, {}, {}, {}]", self.i, self.j, self.k, self.r)
    }
}

impl Neg for Quaternion {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self { i: -self.i, j: -self.j, k: -self.k, r: self.r }
    }
}

impl Mul for Quaternion {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            i: (self.i * rhs.r) + (self.j *-rhs.k) + (self.k * rhs.j) + (self.r * rhs.i),
            j: (self.i * rhs.k) + (self.j * rhs.r) + (self.k *-rhs.i) + (self.r * rhs.j),
            k: (self.i *-rhs.j) + (self.j * rhs.i) + (self.k * rhs.r) + (self.r * rhs.k),
            r: (self.i *-rhs.i) + (self.j *-rhs.j) + (self.k *-rhs.k) + (self.r * rhs.r)
        }
    }
}
impl Quaternion {
    pub fn new(i: f32, j: f32, k: f32, r: f32) -> Self {
        Self { i, j, k, r }
    }
    pub fn axis(x: f32, y: f32, z: f32, angle: f32) -> Self {
        let alpha = angle * 0.5;
        let (s, c) = alpha.sin_cos();
        Self { i: x * s, j: y * s, k: z * s, r: c }
    }
    pub fn ijk_vec(&self) -> Point3 {
        Point3 { x: self.i, y: self.j, z: self.k }
    }
    pub fn normalized(self) -> Self {
        let Self { i, j, k, r } = self;
        let length = (i*i + j*j + k*k + r*r).sqrt().recip();
        Self {
            i: i * length,
            j: j * length,
            k: k * length,
            r: r * length,
        }
    }
    pub fn inverted(&self) -> Self {
        Self {
            i: -self.i,
            j: -self.j,
            k: -self.k,
            r: self.r
        }
    }
    pub fn from_vectors_maybe(umm: Point3, erm: Point3) -> Self {
        let (umm, erm) = (umm.normal(), erm.normal());
        let eek = umm.cross(erm);
        let maybe = umm.dot(erm);
        Self {
            i: eek.x, j: eek.y, k: eek.z,
            r: maybe,
        }
    }
    pub fn from_axis_angle(axis: Point3, angle: f32) -> Self {
        let axis = axis.normal();
        let (sin_alpha, cos_alpha) = (angle * 0.5).sin_cos();
        Self {
            i: axis.x * sin_alpha,
            j: axis.y * sin_alpha,
            k: axis.z * sin_alpha,
            r: cos_alpha,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct Point2 {
    pub x: f32,
    pub y: f32,
}

macro_rules! point2_binvec {
    ($op: ident, $fn: ident) => {
        impl core::ops::$op for Point2 {
            type Output = Point2;
            fn $fn(self, rhs: Self) -> Self::Output {
                Self { x: self.x.$fn(rhs.x), y: self.y.$fn(rhs.y) }
            }
        }
    };
}
macro_rules! point2_binscalar {
    ($op: ident, $fn: ident) => {
        impl core::ops::$op<f32> for Point2 {
            type Output = Point2;
            fn $fn(self, rhs: f32) -> Self::Output {
                Self { x: self.x.$fn(rhs), y: self.y.$fn(rhs) }
            }
        }
    };
}
macro_rules! point2_assignop {
    ($op: ident, $assign: ident, $ty: ty, $fn: ident) => {
        impl $op<$ty> for Point2 {
            fn $assign(&mut self, rhs: $ty) { *self = self.$fn(rhs) }
        }
    };
}
point2_binvec!(Add, add);
point2_binvec!(Sub, sub);
point2_binvec!(Mul, mul);
point2_binscalar!(Add, add);
point2_binscalar!(Sub, sub);
point2_binscalar!(Mul, mul);
point2_assignop!(AddAssign, add_assign, Point2, add);
point2_assignop!(SubAssign, sub_assign, Point2, sub);
point2_assignop!(MulAssign, mul_assign, Point2, mul);
point2_assignop!(AddAssign, add_assign, f32, add);
point2_assignop!(SubAssign, sub_assign, f32, sub);
point2_assignop!(MulAssign, mul_assign, f32, mul);

impl Neg for Point2 {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self { x: -self.x, y: -self.y }
    }
}
impl Point2 {
    pub fn new(x: f32, y: f32) -> Self {
        Self { x, y }
    }
    pub fn sqrt(self) -> Self {
        Self { x: self.x.sqrt(), y: self.y.sqrt() }
    }
    pub fn abs(self) -> Self {
        Self { x: self.x.abs(), y: self.y.abs() }
    }
    pub fn dot(self, rhs: Self) -> f32 {
        self.x * rhs.x + self.y * rhs.y
    }
    pub fn crossed(self) -> Self {
        Self { x: -self.y, y: self.x }
    }
    pub fn length2(self) -> f32 {
        self.dot(self)
    }
    pub fn length(self) -> f32 {
        self.length2().max(0.00001).sqrt()
    }
    pub fn distance2(self, rhs: Self) -> f32 {
        (self - rhs).length2()
    }
    pub fn distance(self, rhs: Self) -> f32 {
        self.distance2(rhs).sqrt()
    }
    pub fn normal(self) -> Self {
        self * self.length().recip()
    }
}

impl Display for Point2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:6.4} {:6.4}]", self.x, self.y)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, PartialOrd)]
pub struct Point3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Display for Point3 {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "[{:6.3}, {:6.3}, {:6.3}]", self.x, self.y, self.z)
    }
}

macro_rules! point3_binvec {
    ($op: ident, $fn: ident) => {
        impl $op for Point3 {
            type Output = Point3;
            fn $fn(self, rhs: Self) -> Self::Output {
                Self {
                    x: self.x.$fn(rhs.x),
                    y: self.y.$fn(rhs.y),
                    z: self.z.$fn(rhs.z),
                }
            }
        }
    };
}
macro_rules! point3_binscalar {
    ($op: ident, $fn: ident) => {
        impl $op<f32> for Point3 {
            type Output = Point3;
            fn $fn(self, rhs: f32) -> Self::Output {
                Self {
                    x: self.x.$fn(rhs),
                    y: self.y.$fn(rhs),
                    z: self.z.$fn(rhs),
                }
            }
        }
    };
}
macro_rules! point3_assignop {
    ($op: ident, $assign: ident, $ty: ty, $fn: ident) => {
        impl $op<$ty> for Point3 {
            fn $assign(&mut self, rhs: $ty) { *self = self.$fn(rhs) }
        }
    };
}
point3_binvec!(Add, add);
point3_binvec!(Sub, sub);
point3_binvec!(Mul, mul);
point3_binscalar!(Add, add);
point3_binscalar!(Sub, sub);
point3_binscalar!(Mul, mul);
point3_assignop!(AddAssign, add_assign, Point3, add);
point3_assignop!(SubAssign, sub_assign, Point3, sub);
point3_assignop!(MulAssign, mul_assign, Point3, mul);
point3_assignop!(AddAssign, add_assign, f32, add);
point3_assignop!(SubAssign, sub_assign, f32, sub);
point3_assignop!(MulAssign, mul_assign, f32, mul);
impl Neg for Point3 {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self { x: -self.x, y: -self.y, z: -self.z }
    }
}
impl Point3 {
    pub const POS_Y: Point3 = Point3 { x: 0.0, y: 1.0, z: 0.0 };
    pub const NEG_Y: Point3 = Point3 { x: 0.0, y: -1.0, z: 0.0 };
    pub const POS_Z: Point3 = Point3 { x: 0.0, y: 0.0, z: 1.0 };
    pub const NEG_Z: Point3 = Point3 { x: 0.0, y: 0.0, z: -1.0 };
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Self { x, y, z }
    }
    pub fn dot(self, rhs: Self) -> f32 {
        let dot = self * rhs;
        dot.x + dot.y + dot.z
    }
    pub fn cross(self, rhs: Self) -> Self {
        Self {
            x: self.y * rhs.z - self.z * rhs.y,
            y: self.z * rhs.x - self.x * rhs.z,
            z: self.x * rhs.y - self.y * rhs.x,
        }
    }
    pub fn length2(self) -> f32 {
        self.dot(self)
    }
    pub fn length(self) -> f32 {
        self.length2().sqrt()
    }
    pub fn distance2(self, rhs: Self) -> f32 {
        (self - rhs).length2()
    }
    pub fn normal(self) -> Self {
        self * self.length().recip()
    }
}

