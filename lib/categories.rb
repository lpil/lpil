# Builds full category path strings
class Categories < Hash
  def initialize(pf_cats)
    get_all_paths! pf_cats
  end

  private

  def get_all_paths!(cats)
    cats.each do |c|
      c[1][:Path] = get_path!(cats, c[1]) unless c[1][:Path]
      self[c[0]] = c[1]
    end
  end

  def get_path!(cats, c)
    if c[:Path]
      c[:Path]
    elsif c[:ParentCategoryID__IDREF]
      c[:Path] = [
        get_path!(cats, cats[(c[:ParentCategoryID__IDREF].to_sym)]),
        c[:DisplayName__STR]
      ].join ' > '
    else
      c[:Path] = c[:DisplayName__STR]
    end
  end
end
