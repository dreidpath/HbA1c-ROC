load("data.RData")
library("xtable")

mat <- matrix(c(0.9, 0.89, 200, 0.045), c(2, 2))
rownames(mat) <- c("$y_{t-1}$", "$y^{t-1}$")
colnames(mat) <- c("$R^2$", "$\\bar{x}$")
mat1 <- xtable(mat)
print(mat1, type="html", sanitize.text.function = function(x) {x})



<table>
  <tr>
  <th></th>
  <th colspan="2">Actual<br></th>
  </tr>
  <tr>
  <td>Predicted</td>
  <td>Positive<br></td>
  <td>Negative</td>
  </tr>
  <tr>
  <td>Positive</td>
  <td>123</td>
  <td>124</td>
  </tr>
  <tr>
  <td>Negative</td>
  <td>321</td>
  <td>421</td>
  </tr>
</table>