class Complement
  def self.of_dna(strand)
    new.of_dna strand
  end

  def self.of_rna(strand)
    new.of_rna strand
  end

  def of_dna(strand)
    map_bases(strand, :to_rna)
  end

  def of_rna(strand)
    map_bases(strand, :to_dna)
  end

  private

  def map_bases(strand, function)
    strand.split('')
      .map { |base| send function, base }
      .join('')
  end

  def to_rna(dna_base)
    case dna_base
    when 'G'
      'C'
    when 'C'
      'G'
    when 'T'
      'A'
    when 'A'
      'U'
    end
  end

  def to_dna(rna_base)
    case rna_base
    when 'C'
      'G'
    when 'G'
      'C'
    when 'A'
      'T'
    when 'U'
      'A'
    end
  end
end
