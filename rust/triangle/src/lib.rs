#[derive(Debug,PartialEq)]
pub enum TriangleType {
    Equalateral,
    Isosceles,
    Scalene,
}

pub struct Triangle(TriangleType);

impl Triangle {
    pub fn build(sides: [usize; 3]) -> Result<Triangle, String> {
        let mut sorted = sides.clone();
        sorted.sort();
        let big = sorted[2];
        let med = sorted[1];
        let sml = sorted[0];

        if sml < 1 || big >= med + sml {
            Err(String::from("Invalid sides"))
        } else if big == med && med == sml {
            Ok(Triangle(TriangleType::Equalateral))
        } else if big == med || med == sml || sml == big {
            Ok(Triangle(TriangleType::Isosceles))
        } else {
            Ok(Triangle(TriangleType::Scalene))
        }
    }

    pub fn is_equilateral(&self) -> bool {
        self.order() == &TriangleType::Equalateral
    }

    pub fn is_isosceles(&self) -> bool {
        self.order() == &TriangleType::Isosceles
    }

    pub fn is_scalene(&self) -> bool {
        self.order() == &TriangleType::Scalene
    }

    pub fn order(&self) -> &TriangleType {
        let &Triangle(ref value) = self;
        value
    }
}
