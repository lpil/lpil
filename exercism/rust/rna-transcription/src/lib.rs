pub type Base = char;
pub type Strand = String;

#[derive(Debug)]
#[derive(PartialEq)]
pub struct RibonucleicAcid(Strand);

impl RibonucleicAcid {
    pub fn new(strand: &str) -> RibonucleicAcid {
        RibonucleicAcid(String::from(strand))
    }
}


#[derive(Debug)]
pub struct DeoxyribonucleicAcid(Strand);

impl DeoxyribonucleicAcid {
    pub fn new(strand: &str) -> DeoxyribonucleicAcid {
        DeoxyribonucleicAcid(String::from(strand))
    }

    pub fn to_rna(&self) -> RibonucleicAcid {
        let DeoxyribonucleicAcid(ref dna_stand) = *self;
        let rna_strand = dna_stand.chars()
            .flat_map(dna_base_to_rna_base)
            .collect::<String>();
        RibonucleicAcid(rna_strand)
    }
}


fn dna_base_to_rna_base(c: Base) -> Option<Base> {
    match c {
        'C' => Some('G'),
        'A' => Some('U'),
        'T' => Some('A'),
        'G' => Some('C'),
        _ => None,
    }
}
