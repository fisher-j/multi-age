-- Nest an included document's headings without modifying its Markdown source.
function Div(div)
  local offset = tonumber(div.attributes["heading-offset"])
  if offset then
    return div:walk({
      Header = function(header)
        header.level = math.min(6, header.level + offset)
        return header
      end
    })
  end
end
